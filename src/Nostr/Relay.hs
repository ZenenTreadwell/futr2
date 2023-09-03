module Nostr.Relay where 

-- import Nostr.Db
import Prelude as P
import Control.Exception as E
import Control.Concurrent.STM.TChan
import Network.WebSockets as WS
import Database.SQLite.Simple as SQL
import Data.Aeson as J
import Data.ByteString.Lazy as LB
import Data.List 
import Control.Monad 
import Control.Monad.STM
import Control.Concurrent
import Nostr.Wire
import qualified Data.ByteString.Base16 as Hex
import Nostr.Beam
import Nostr.Event 
import Nostr.Filter 


type Listen = IO (Either WS.ConnectionException LB.ByteString)

relay :: SQL.Connection -> TChan Event -> ClientApp () 
relay db chan ws = forever do
    print "server"
    eo <- E.try . WS.receiveData $ ws :: Listen
    case decode <$> eo of 
        Right (Just d) -> case d of 
            Subscribe s fx -> do 
                ex <- fetchx db fx
                void $ WS.sendTextDatas ws $ P.map (encode . See s) ex   
                myChan <- atomically $ dupTChan chan
                void . forkIO $ forever do 
                     e <- atomically $ readTChan myChan
                     if P.any (\f -> matchF e f) fx 
                         then WS.sendTextData ws . encode $ See s e
                         else pure () 
            Submit e -> do 
                trust <- verifyE e 
                when trust (mask_ $ insertEv db e)  
            End s -> pure () --myThreadId >>= killThread 
        Right Nothing -> print . (<> " - nothing") . show $ eo
        Left z -> print z >> myThreadId >>= killThread 


fetchx :: SQL.Connection -> [Filter] -> IO [Event]
fetchx db fx = nub . mconcat <$> mapM (fetch db) fx 




