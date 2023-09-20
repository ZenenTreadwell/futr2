module Nostr.Relay where 

import Prelude as P
import qualified Data.Text as T
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
import Nostr.Auth
import Nostr.Keys
import System.Entropy
import Control.Monad.Trans.Maybe
import Data.Text.Encoding
import qualified Data.ByteString.Base16 as Hex

type Listen = IO (Either WS.ConnectionException LB.ByteString)
type Subs = M.Map Text [Filter] 
-- type Auth = M.Map Hex32 

data Nctx = Nctx {
      database :: SQL.Connection
    , eventfeed :: TChan Event
    , subs :: [TVar Subs]
    -- , 
    }

relay :: SQL.Connection -> TChan Event -> ClientApp () 
relay db chan ws = do
    print "client connected"
    s <- newTVarIO M.empty
    r <- decodeUtf8 . Hex.encode <$> getEntropy 32
    WS.sendTextData ws . encode $ Challenge r
    race_ (listen' r s) (broadcast' s) 
    where 

    broadcast' :: TVar Subs -> IO () 
    broadcast' subs = forever do 
        e <- atomically $ readTChan chan
        m <- readTVarIO subs
        print "broadcasting!"
        case findKeyByValue (P.any (matchF e)) m of
            Just s' -> WS.sendTextData ws . encode $ See s' e
            _ -> pure () 

    listen' :: Text -> TVar Subs -> IO ()
    listen' c subs = forever do
        eo <- E.try . WS.receiveData $ ws :: Listen
        case decode <$> eo of 
            Right (Just d) -> case d of 
                Subscribe s fx -> do 
                    ex <- fetchx db fx
                    void $ WS.sendTextDatas ws 
                         $ P.map (encode . See s) ex   
                    atomically $ modifyTVar subs (M.insert s fx) 
                Submit e -> submit db ws e 
                End s -> atomically $ modifyTVar subs (M.delete s)
                Auth t -> do 
                    v <- runMaybeT $ validate t c
                    case v of 
                        Just p -> print p
                        Nothing -> print "failly"
            Right Nothing -> print . (<> " - right nothing") . show $ eo
            Left z -> do
                print z 
                print "client killed_"  
                myThreadId >>= killThread 

fetchx :: SQL.Connection -> [Filter] -> IO [Event]
fetchx db fx = nub . mconcat <$> mapM (fetch db) fx 

findKeyByValue :: (a -> Bool) -> M.Map k a -> Maybe k
findKeyByValue f = M.foldlWithKey' (\acc k v -> 
    if f v then Just k else acc) Nothing

submit :: SQL.Connection -> WS.Connection -> Event -> IO () 
submit db ws e = do 
    trust <- verifyE e
    if not trust 
        then reply False (Invalid "signature failed")
        else do  
            insRes <- (mask_ $ insertEv db e)  
            case insRes of 
                Left (SQLError ErrorConstraint t _) -> 
                    reply False (Duplicate t) 
                Left (SQLError _ t _) -> 
                    reply False (ServerErr t)
                Right _ -> reply True None 
    
    where
    reply :: Bool -> WhyNot -> IO () 
    reply b = WS.sendTextData ws . encode . Ok (eid e) b  