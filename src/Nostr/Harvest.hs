module Nostr.Harvest where 

-- import Prelude as P
-- import Control.Exception as E
-- import Control.Concurrent
-- import Network.WebSockets as WS
-- import Network.Connection as C 
-- import Network.TLS as TLS
-- import Wuss
-- import Database.SQLite.Simple as SQL
-- import Text.URI as URI
-- import Data.Text as T
-- import Data.Time.Clock.POSIX
-- import Control.Concurrent.STM.TChan
-- import Control.Concurrent.Async
-- import Control.Monad.Trans.Maybe
-- import GHC.Conc
-- import Data.Aeson
-- import Control.Monad
-- import Nostr.Event
-- import Nostr.Wire 
-- import Nostr.Filter 
-- import Nostr.Beam
-- import Nostr.Keys
-- import Nostr.Auth

-- harvestr :: SQL.Connection -> URI -> Maybe (IO ()) 
-- harvestr db uri = do 
--     sch <- unRText <$> uriScheme uri 
--     (host, port, path) <- extractURI uri
--     handle conerr2 
--         . handle handerr 
--         . handle tlserr 
--         . handle urierr 
--         . handle cryerr 
--         . handle cryerr3
--         <$> 
--         case sch of 
--             "wss" -> Just $ runSecureClient host (fromIntegral port) path cli 
--             "ws"  -> Just $ WS.runClient host (fromIntegral port) path cli
--             _ -> Nothing
--     where 
--     caught :: Exception z => z -> IO () 
--     caught z = print . ("caught caught... " <>) . P.take 227 . show $ z
--     urierr :: URI.ParseException -> IO () 
--     urierr = caught 
--     tlserr :: TLS.TLSException -> IO () 
--     tlserr = caught
--     conerr2 :: C.HostCannotConnect -> IO () 
--     conerr2 = caught 
--     handerr :: WS.HandshakeException -> IO () 
--     handerr = caught 
--     cryerr :: WS.ConnectionException -> IO () 
--     cryerr = caught 
--     cryerr3 :: IOError -> IO ()
--     cryerr3 = caught 
--     cli :: ClientApp ()
--     cli conn = do
--         sec :: Integer <- round <$> getPOSIXTime
--         subscribe "a" conn [liveF sec]             
--         forever $  harvest db uri conn     

-- createFeeder :: URI -> IO Feeder -- ClientApp () 
-- createFeeder d = Feeder d <$> a <*> a
--     where a = newTChanIO 

-- data Feeder = Feeder {
--       furi :: URI
--     , feedUp :: TChan Up
--     , feedDown :: TChan Down
--     } 

-- nomNom :: Feeder -> Maybe (IO (ThreadId))
-- nomNom ff@(Feeder uri u d) = do 
--     sch <- unRText <$> uriScheme uri 
--     (host, port, path) <- extractURI uri
--     forkIO <$> case sch of 
--         "wss" -> Just $ runSecureClient host (fromIntegral port) path cli  
--         "ws"  -> Just $ WS.runClient host (fromIntegral port) path cli
--         _ -> Nothing

--     where 
--     cli = feeder ff


-- feeder :: Feeder -> ClientApp ()  
-- feeder (Feeder{..}) ws = race_ (forever broadcast) (forever acceptcast)   
--     where 
--     broadcast =  atomically (readTChan feedUp) >>= 
--                      WS.sendTextData ws . encode

--     acceptcast = receiveData ws >>= \c -> case decode c of 
--         Just dow -> atomically (writeTChan feedDown dow) 
--         _ -> print "decode failed" >> print c


-- harvest :: SQL.Connection -> URI ->  ClientApp ()
-- harvest db uri ws = catch rec conerr
--     where 
--     conerr :: ConnectionException -> IO () 
--     conerr z = do 
--         print $ "harvestr catch n kill " <> show z  
--         myThreadId >>= killThread
            
--     rec = do 
--         mdown <- receiveData ws 
--         case decode mdown of 
--             Just town -> goDown town
--             Nothing -> print mdown >> print "decode failed?" 
--         where
--         goDown = \case  
--             See _ e@(Event _ _ (Content{kind})) -> do  
--                 trust <- verifyE e 
--                 when trust do
--                     -- XXX bypasses dmt
--                     scs <- insertEv db e
--                     case scs of 
--                         Left l -> pure () 
--                         Right r -> print "-- --" 
--             Live _ -> print "--------live"
--             Ok _ b c  -> print $ "ok? " <> show b <> (show.toJSON) c
--             Notice note -> print $ "note:" <> note 
--             Challenge t -> do
--                 kp <- genKeyPair
--                 e <- authenticate kp uri t
--                 WS.sendTextData ws . encode $ Auth e  
--                 print "sent auth (rec challenge)"
--             CountD _ _ -> pure () 

-- extractURI :: URI -> Maybe (String, Word, String)
-- extractURI uri = do 
--     Right a <- pure $ uriAuthority uri 
--     let host = T.unpack . unRText $ authHost a
--     defaultport <- case unRText <$> uriScheme uri of
--         (Just "wss") -> Just 443
--         _ -> Just 80
--     let port = maybe defaultport id $ authPort a
--     let path = T.unpack . maybe "/" joinpath $ uriPath uri
--     pure (host, port, path)
--     where 
--     joinpath (trailingSlash, rx) = if not trailingSlash 
--         then joined `T.append` "/"
--         else joined 
--         where joined = P.foldl T.append "" $ 
--                        fmap (flip T.append "/" . unRText) rx  

-- subscribe :: Text -> WS.Connection -> [Filter] -> IO ()
-- subscribe a conn = WS.sendTextData conn . encode . Subscribe a

