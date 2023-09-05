module Nostr.Relay where 

-- import Nostr.Db
import Prelude as P
import Control.Exception as E
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Network.WebSockets as WS
import Database.SQLite.Simple as SQL
import Data.Aeson as J
import Data.ByteString.Lazy as LB
import Data.List 
import Data.Text (Text)
import Data.Map.Strict as M 
import Control.Monad 
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Nostr.Wire
import Nostr.Beam
import Nostr.Event 
import Nostr.Filter 

type Listen = IO (Either WS.ConnectionException LB.ByteString)
type Subs = M.Map Text [Filter] 

relay :: SQL.Connection -> TChan Event -> ClientApp () 
relay db chan ws = do
    s <- newTVarIO M.empty
    race_ (listen' s) (broadcast' s) 
    where 
    listen' :: TVar Subs -> IO ()
    listen' subs = forever do 
        eo <- E.try . WS.receiveData $ ws :: Listen
        case decode <$> eo of 
            Right (Just d) -> case d of 
                Subscribe s fx -> do 
                    ex <- fetchx db fx
                    void $ WS.sendTextDatas ws 
                         $ P.map (encode . See s) ex   
                    atomically $ modifyTVar subs (M.insert s fx) 
                Submit e -> do 
                    trust <- verifyE e 
                    when trust (mask_ $ insertEv db e)  
                End s -> atomically $ modifyTVar subs (M.delete s)  
            Right Nothing -> print . (<> " - nothing") . show $ eo
            Left z -> print z >> myThreadId >>= killThread 
    
    broadcast' :: TVar Subs -> IO () 
    broadcast' subs = forever do 
        e <- atomically $ readTChan chan
        m <- readTVarIO subs
        case findKeyByValue (P.any (matchF e)) m of
            Just s' -> WS.sendTextData ws . encode $ See s' e
            _ -> pure () 

fetchx :: SQL.Connection -> [Filter] -> IO [Event]
fetchx db fx = nub . mconcat <$> mapM (fetch db) fx 

findKeyByValue :: (a -> Bool) -> M.Map k a -> Maybe k
findKeyByValue f = M.foldlWithKey' (\acc k v -> 
    if f v then Just k else acc) Nothing
