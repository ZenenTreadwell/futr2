module Nostr.Pool where 

import Prelude as P
import Control.Concurrent
import Control.Exception
import Network.WebSockets as WS
import Wuss
import Database.SQLite.Simple as SQL
import Network.Connection as C 
import Network.TLS as TLS
import Text.URI as URI
import Data.Text as T
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
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
import Data.Time.Clock.POSIX
import Nostr.Boots

poolParty :: SQL.Connection -> Hex96 -> IO Pool 
poolParty db kp = do 
    p <- Pool <$>  newTVarIO M.empty
    tc <- newTChanIO
    let wr = (tc, db) 
    let pool = p wr kp
    sec :: Integer <- round <$> getPOSIXTime
    mapM_ (addRelayP pool) (defaultRelay)
    u <- exportPub kp
    void . forkIO $ insertLoop tc
    castAll pool $ Subscribe "a" [ 
          liveF sec 
        , emptyF{ptagF=Just (PTagM [u])}
        ]
    pure pool

type Pool' = M.Map URI Feed 

data Pool = Pool (TVar Pool') WriteReadDb Hex96

instance Eq Pool where 
    (==) _ _ = False
    
data Feed = Feed {
      sendchan :: (TChan Up) 
    , activethread :: ThreadId 
    } deriving Eq

addRelayP :: Pool -> URI -> IO ()
addRelayP (Pool p db kp) uri = do 
    skr <- case checkUri uri of 
        Just socker -> pure socker 
        _ -> error "invalid uri?"
    ch <- newTChanIO
    trd <- forkIO . gottaCatchemAll uri p . skr $ feeder kp uri ch db
    atomically $ modifyTVar p (M.insert uri (Feed ch trd))
    
feeder :: Hex96 -> URI -> TChan Up -> WriteReadDb -> ClientApp ()  
feeder kp uri ch (wr, db) ws = race_ (forever broadcast) (forever acceptcast)   
    where 
    pri x = print $ render uri <> "  " <> x
    
    broadcast =  atomically (readTChan ch) 
                     >>= WS.sendTextData ws . encode

    acceptcast = receiveData ws >>= \c -> do 
        pri "ce"
        case decode c of 
            Just dow -> downer dow  
            _ -> error " feeder, acceptcast, Pool " 
    
    downer :: Down -> IO ()
    downer d =  
        case d of   
            See _ e -> do  
                trust <- verifyE e 
                when trust do 
                    pri "ev"
                    atomically . writeTChan wr . void 
                        $ insertEv db e
                    atomically . writeTChan wr 
                        $ insertOrigin db uri (eid e)
            Live l -> print $ "--------live " <> l
            Ok _ b c  -> pri $ "ok? " <> T.pack (P.show c)
            Notice note -> pri $ "note:" <> note 
            Challenge t -> do
                e <- authenticate kp uri t
                atomically $ writeTChan ch (Auth e)  
            CountD _ _ -> pure () 
     
byeRelay :: Pool -> URI -> IO ()
byeRelay (Pool tv _ _) uri = do
    tv' <- readTVarIO tv  
    for_ (M.lookup uri tv') \(Feed _ t) -> do  
        atomically . writeTVar tv $ M.delete uri tv'
        killThread t 

castAll :: Pool -> Up -> IO () 
castAll (Pool tv _ _) u = do 
    tv' <- readTVarIO tv  
    mapM_ xyz tv'  
    where xyz :: Feed -> IO () 
          xyz (Feed uch _) = atomically $ writeTChan uch u 
    
castOne :: Pool -> URI -> Up -> IO () 
castOne (Pool tv _ _) uri m = do 
    tv' <- readTVarIO tv  
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
    , kindsF = Just $ Kinds [1] 
    , limitF = Just $ Limit 0 
    } 

directF :: Hex32 -> Filter 
directF p = emptyF {
      ptagF = Just $ PTagM [p]
    , kindsF = Just $ Kinds [4] 
    } 

gottaCatchemAll :: URI -> TVar Pool' -> IO () -> IO ()  
gottaCatchemAll uri tv = 
      handle conerr2 
    . handle handerr 
    . handle tlserr 
    . handle urierr 
    . handle cryerr 
    . handle cryerr3
    where 
    caught :: String -> Exception z => z -> IO () 
    caught zz z = do 
        atomically $ modifyTVar tv (M.delete uri)
        print . (zz <>)
              . ("caught caught... " <>) 
              . P.take 227 . show $ z
    urierr :: URI.ParseException -> IO () 
    urierr = caught  showrender
    tlserr :: TLS.TLSException -> IO () 
    tlserr = caught showrender
    conerr2 :: C.HostCannotConnect -> IO () 
    conerr2 = caught  showrender
    handerr :: WS.HandshakeException -> IO () 
    handerr = caught  showrender
    cryerr :: WS.ConnectionException -> IO () 
    cryerr = caught  showrender
    cryerr3 :: IOError -> IO ()
    cryerr3 = caught showrender
    showrender = show . render $ uri
