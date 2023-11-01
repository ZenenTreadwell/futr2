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
import Nostr.Beam
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

type Nip11 = Get '[JSON] Text 
n11 :: Server Nip11
n11 = do 
    return . decodeUtf8 . BS.toStrict . encode . object $ [ 
          "name" .=+ ""
        , "description" .=+ "" 
        , "pubkey" .=+ "" 
        , "contact" .=+ ""
        , "supported_nips" .= 
             ([1, 2, 4, 9, 10, 45, 42, 40, 12, 16, 20, 33]
              :: [Int])
        , "software" .=+ "http://github.com/autonomousorganization/futr2"  
        , "version" .=+ "0.0.0.0"
        ]
    where 
    (.=+) :: KeyValue k => Key -> Text -> k 
    (.=+) = (.=)

runRelay :: Port -> SQL.Connection -> TChan Event -> IO ()
runRelay p o f = W.run p $ websocketsOr defaultConnectionOptions 
    (acceptRequest >=> relay o f) 
    (serve (Proxy :: Proxy Nip11) n11) 


relay :: SQL.Connection -> TChan Event -> ClientApp () 
relay db chan ws = do
    chan' <- atomically $ dupTChan chan
    s <- newTVarIO M.empty
    r <- decodeUtf8 . Hex.encode <$> getEntropy 32
    a <- newTVarIO (Left r)
    WS.sendTextData ws . encode $ Challenge r
    race_ (listen' a s) (broadcast' a s chan') 
    
    where 

    broadcast' :: TVar Auth -> TVar Subs -> TChan Event -> IO () 
    broadcast' au subs chan = forever do 
        e <- atomically $ readTChan chan
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
                    atomically $ modifyTVar subs (M.insert s fx) 
                Submit e -> submit db ws e 
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
                    n <- sum . L.concatMap (P.map fromIntegral) -- sum . L.concat . P.map fromInteger 
                        <$> mapM (countFx db) fx 
                    void $ WS.sendTextData ws . encode 
                         $ CountD s n 
            Right Nothing -> print . (<> " - right nothing") . show $ eo
            Left z -> do
                print z 
                print "client killed_"  
                myThreadId >>= killThread 

isSend :: Auth -> Event -> Bool
isSend _ (Event _ _ Content{kind}) | kind /= 4 = True
isSend (Left _) _ = False
isSend (Right p) (Event _ _ Content{tags, pubkey}) = case P.filter isPTag tags of 
    (PTag x _ _) : _ -> x == p || pubkey == p
    _ -> False
    where 
    isPTag (PTag{}) = True
    isPTag _ = False    
    

fetchx :: SQL.Connection -> [Filter] -> IO [Event]
fetchx db fx = nub . mconcat <$> mapM (fetch db) fx 

findKeyByValue :: (a -> Bool) -> M.Map k a -> Maybe k
findKeyByValue f = M.foldlWithKey' (\acc k v -> 
    if f v then Just k else acc) Nothing

submit :: SQL.Connection -> WS.Connection -> Event -> IO () 
submit db ws e = do 
    trust <- verifyE e
    if not trust then reply False (Invalid "signature failed")
    else do  
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