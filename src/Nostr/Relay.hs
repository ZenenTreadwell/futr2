module Nostr.Relay where 

import Prelude as P
import Control.Exception as E
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Network.WebSockets as WS
import Database.SQLite.Simple as SQL
import Control.Monad.IO.Class
import Data.Aeson as J
import Data.ByteString.Lazy as LB
import Data.ByteString as BS
import Data.List as L
import Data.Text (Text)
import Data.Map.Strict as M 
import Control.Monad 
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Nostr.Wire
import Nostr.Db.Schema
import Nostr.Db.Insert
import Nostr.Db.Fetch
import Nostr.Event 
import Nostr.Filter 
import Nostr.Auth
import Nostr.Keys
import System.Entropy
import Control.Monad.Trans.Maybe
import Data.Text.Encoding
import qualified Data.ByteString.Base16 as Hex
import Servant.API
import Servant.Server
import Data.Proxy
import Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WebSockets

type Listen = IO (Either WS.ConnectionException LB.ByteString)
type Subs = M.Map Text [Filter] 
type Auth = Either Text Hex32

data RelayConf = RC { 
      rc_name :: Text 
    , rc_contact :: Text 
    , rc_description :: Text 
    , rc_port :: Port 
    , rc_pubkey :: Hex32 
    } deriving (Show)

type Nip11 = Get '[JSON] Text 



nip11 :: RelayConf -> Value
nip11 (RC n d c _ k) = object [
      "name" .=+ n
    , "description" .=+ d 
    , "pubkey" .=+ wq k 
    , "contact" .=+ c
    , "supported_nips" .= 
         ([1, 2, 4, 9, 10, 45, 42, 40, 12, 16, 20, 33]
          :: [Int])
    , "software" .=+ "autonomousorganization/futr2"  
    , "version" .=+ "0.0.0.0"
    ] 
    where 
    (.=+) :: KeyValue Value k => Key -> Text -> k 
    (.=+) = (.=)

n11 :: RelayConf -> Server Nip11
n11 = return . decodeUtf8 . BS.toStrict . encode . nip11 

runRelay :: RelayConf -> WriteReadDb -> TChan Event -> IO ()
runRelay conf@(RC _ _ _ p _) o f =  
    W.run p $ websocketsOr defaultConnectionOptions 
        (acceptRequest >=> relay o f) 
        (serve (Proxy :: Proxy Nip11) (n11 conf)) 

relay :: WriteReadDb -> TChan Event -> ClientApp () 
relay (wr, db) chan ws = do
    chan' <- atomically $ dupTChan chan
    s <- newTVarIO M.empty
    r <- decodeUtf8 . Hex.encode <$> getEntropy 32
    a <- newTVarIO (Left r)
    WS.sendTextData ws . encode $ Challenge r
    race_ (listen' a s) (broadcast' a s chan') 
    
    where 

    broadcast' :: TVar Auth -> TVar Subs -> TChan Event -> IO () 
    broadcast' au subs tc = forever do 
        e <- atomically $ readTChan tc
        m <- readTVarIO subs
        a <- readTVarIO au
        when (isSend a e) case findKeyByValue (P.any (matchF e)) m of
            Just s' -> WS.sendTextData ws . encode $ See s' e
            _ -> pure () 

    listen' :: TVar Auth -> TVar Subs -> IO ()
    listen' au subs = forever do
        eo <- E.try . WS.receiveData $ ws :: Listen
        case decode <$> eo of 
            Right (Just d) -> case d of 
                Subscribe s fx -> do 
                    ex <- fetchx db fx
                    a <- readTVarIO au
                    void $ WS.sendTextDatas ws 
                         $ P.map (encode . See s) $ P.filter (isSend a) ex   
                    void $ WS.sendTextData ws . encode $ Live s
                    atomically $ modifyTVar subs (M.insert s fx) 
                Submit e -> submit (wr, db) ws e 
                End s -> atomically $ modifyTVar subs (M.delete s)
                Auth t -> do 
                    a <- readTVarIO au
                    case a of 
                        Right _ -> pure () 
                        Left c -> do 
                            v <- runMaybeT $ validate t c
                            case v of 
                                Just p -> atomically $ writeTVar au (Right p)
                                Nothing -> pure () 
                CountU s fx -> do 
                    n <- sum . L.concatMap (P.map fromIntegral)  
                        <$> mapM (countFx db) fx 
                    void $ WS.sendTextData ws . encode 
                         $ CountD s n 
            Right Nothing -> print . (<> " - right nothing") . show $ eo
            Left _ -> myThreadId >>= killThread 

isSend :: Auth -> Event -> Bool
isSend _ (Event _ _ Content{kind}) | kind /= 4 = True
isSend (Left _) _ = False
isSend (Right p) (Event _ _ Content{tags, pubkey}) = case P.filter isPTag tags of 
    (PTag x _ _) : _ -> x == p || pubkey == p
    _ -> False
    where 
    isPTag (PTag{}) = True
    isPTag _ = False    
    

findKeyByValue :: (a -> Bool) -> M.Map k a -> Maybe k
findKeyByValue f = M.foldlWithKey' (\acc k v -> 
    if f v then Just k else acc) Nothing

submit :: WriteReadDb -> WS.Connection -> Event -> IO () 
submit (wr, db)  ws e = do 
    trust <- verifyE e
    if not trust then reply False (Invalid "signature failed")
    else atomically $ writeTChan wr do  
        insRes <- mask_ $ insertEv db e  
        case insRes of 
            Left (SQLError ErrorConstraint t _) -> 
                reply False (Duplicate t) 
            Left (SQLError _ t _) -> 
                reply False (ServerErr t)
            Right _ -> reply True None 
    where
    reply :: Bool -> WhyNot -> IO () 
    reply b = WS.sendTextData ws . encode . Ok (eid e) b  