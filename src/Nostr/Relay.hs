module Nostr.Relay where 

-- import Nostr.Db
import Control.Exception as E
import Network.WebSockets as WS
import Database.SQLite.Simple as SQL
import Data.Aeson as J
import Data.ByteString.Lazy as LB
import Control.Monad 
import Nostr.Wire
import Nostr.Beam (insertEv)
import Nostr.Event (verifyE)


type Listen = IO (Either WS.ConnectionException LB.ByteString)

relay :: SQL.Connection -> ClientApp () 
relay db ws = forever do
    print "server"
    eo <- E.try . WS.receiveData $ ws :: Listen
    case decode <$> eo of 
        Right (Just d) -> case d of 
            Subscribe s fx -> print "sub"  
            Submit e -> do 
                trust <- verifyE e 
                when trust (mask_ $ insertEv db e)  
            End s -> pure () --myThreadId >>= killThread 
        Right Nothing -> print . (<> " - nothing") . show $ eo
        Left z -> print z 







