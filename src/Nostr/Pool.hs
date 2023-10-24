module Nostr.Pool where 

import Prelude as P
import Control.Exception as E
import Control.Concurrent
import Network.WebSockets as WS
import Network.Connection as C 
import Network.TLS as TLS
import Wuss
import Database.SQLite.Simple as SQL
import Text.URI as URI
import Data.Text as T
import Data.Time.Clock.POSIX
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.Monad.Trans.Maybe
import GHC.Conc
import Data.Aeson
import Control.Monad
import Nostr.Event
import Nostr.Wire 
import Nostr.Filter 
import Nostr.Beam
import Nostr.Keys
import Nostr.Auth
import Data.Map as M
import Data.Foldable

data Pool = Pool (TVar (M.Map URI Feed)) SQL.Connection Hex96

data Feed = Feed (TChan Up) ThreadId

addRelay :: Pool -> URI -> IO ()
addRelay (Pool p db kp) uri = do 
    skr <- case checkUri uri of 
        Just socker -> pure socker 
        _ -> error "invalid uri?"
    ch <- newTChanIO
    trd <- forkIO . skr $ feeder kp uri ch db
    atomically $ modifyTVar p (M.insert uri (Feed ch trd))
    
feeder :: Hex96 -> URI -> TChan Up -> SQL.Connection -> ClientApp ()  
feeder kp uri ch db ws = race_ (forever broadcast) (forever acceptcast)   
    where 
    broadcast =  atomically (readTChan ch) 
                     >>= WS.sendTextData ws . encode

    acceptcast = receiveData ws >>= \c -> case decode c of 
        Just dow -> downer dow  
        _ -> print "decode failed" >> print c
    
    downer :: Down -> IO ()
    downer = \case  
        See _ e@(Event _ _ (Content{kind})) -> do  
            trust <- verifyE e 
            -- XXX kind 
            when trust (void $ insertEv db e)
        Live _ -> print "--------live"
        Ok _ b c  -> print $ "ok? " <> show b <> (show.toJSON) c
        Notice note -> print $ "note:" <> note 
        Challenge t -> do
            e <- authenticate kp uri t
            atomically $ writeTChan ch (Auth e)  
            print "sent auth (rec challenge)"
        CountD _ _ -> pure () 
     
byeRelay :: Pool -> URI -> IO ()
byeRelay p@(Pool tv _ kp) uri = do
    tv' <- atomically $ readTVar tv  
    case M.lookup uri tv' of 
        Just (Feed uch trd) -> do  
            atomically . writeTVar tv $ M.delete uri tv'
            killThread trd 
        _ -> pure () 

castAll :: Pool -> Up -> IO () 
castAll (Pool tv _ _) u = do 
    tv' <- atomically $ readTVar tv  
    mapM_ xyz tv'  
    where xyz :: Feed -> IO () 
          xyz (Feed uch _) = atomically $ writeTChan uch u 
    
castOne :: Pool -> URI -> Up -> IO () 
castOne (Pool tv _ _) uri m = do 
    tv' <- atomically $ readTVar tv  
    case M.lookup uri tv' of 
        Just (Feed uch _) -> atomically $ writeTChan uch m 
        _ -> pure () 

signCastAll :: Pool -> Keyless -> IO () 
signCastAll p@(Pool tv _ kp) kl = do 
    u <- Submit <$> signE kp kl
    castAll p u 

signCastOne :: Pool -> URI -> Keyless -> IO () 
signCastOne p@(Pool tv _ kp) uri kl = do 
    u <- Submit <$> signE kp kl
    castOne p uri u


    
checkUri :: URI -> Maybe (ClientApp () -> IO ())
checkUri uri = do 
    sch <- unRText <$> uriScheme uri 
    (host, port, path) <- extractURI uri
    case sch of 
        "wss" -> Just $ runSecureClient host (fromIntegral port) path   
        "ws"  -> Just $ WS.runClient host (fromIntegral port) path 
        _ -> Nothing

extractURI :: URI -> Maybe (String, Word, String)
extractURI uri = do 
    Right a <- pure $ uriAuthority uri 
    let host = T.unpack . unRText $ authHost a
    defaultport <- case unRText <$> uriScheme uri of
        (Just "wss") -> Just 443
        _ -> Just 80
    let port = maybe defaultport id $ authPort a
    let path = T.unpack . maybe "/" joinpath $ uriPath uri
    pure (host, port, path)
    where 
    joinpath (trailingSlash, rx) = if not trailingSlash 
        then joined `T.append` "/"
        else joined 
        where joined = P.foldl T.append "" $ 
                       fmap (flip T.append "/" . unRText) rx  
                      
liveF :: Integer -> Filter 
liveF sec = emptyF { 
      sinceF = Just $ Since $ sec 
    , kindsF = Just $ Kinds [0, 1] 
    , limitF = Just $ Limit 0 
    } 

directF :: Hex32 -> Filter 
directF p = emptyF {
      ptagF = Just $ PTagM [p]
    , kindsF = Just $ Kinds [4] 
    } 
