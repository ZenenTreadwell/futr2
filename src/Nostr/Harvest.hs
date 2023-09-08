module Nostr.Harvest where 

import Prelude as P
import Control.Exception as E
import Network.WebSockets as WS
import Network.Connection as C 
import Network.TLS as TLS
import Wuss
import Database.SQLite.Simple as SQL
import Text.URI as URI
import Data.Text as T
import Data.Time.Clock.POSIX
import Data.Aeson
import Control.Monad
import Nostr.Event
import Nostr.Wire 
import Nostr.Filter 
import Nostr.Beam


harvestm :: SQL.Connection -> IO () 
harvestm o = do 
    undefined 

harvestr :: SQL.Connection -> URI -> Maybe (IO ()) 
harvestr db uri = do 
    sch <- unRText <$> uriScheme uri 
    (host, port, path) <- extractURI uri
    -- handle errr 
    handle conerr2 
        . handle handerr 
        . handle tlserr 
        . handle urierr 
        . handle cryerr 
        . handle cryerr3
        <$> 
        case sch of 
            "wss" -> Just $ runSecureClient host (fromIntegral port) path ws 
            "ws"  -> Just $ WS.runClient host (fromIntegral port) path ws
            _ -> Nothing
    where 
    -- XXX XXX XXX XXX
    caught :: Exception z => z -> IO () 
    caught z = print . ("caught caught... " <>) . P.take 27 . show $ z
    urierr :: URI.ParseException -> IO () 
    urierr = caught 
    tlserr :: TLS.TLSException -> IO () 
    tlserr = caught
    conerr2 :: C.HostCannotConnect -> IO () 
    conerr2 = caught 
    handerr :: WS.HandshakeException -> IO () 
    handerr = caught 
    cryerr :: WS.ConnectionException -> IO () 
    cryerr = caught 
    cryerr3 :: IOError -> IO () --Network.Socket
    cryerr3 = caught 
            
    ws conn = do
        sec :: Integer <- round <$> getPOSIXTime
        subscribe "a" conn [liveF sec]             
        harvest db conn     

harvest :: SQL.Connection -> ClientApp () 
harvest db ws = catch rec conerr 
    where 
    conerr = \z -> do 
        print $ "harvest catch " <> show z  
        case z :: ConnectionException of  
            ConnectionClosed -> pure ()
            WS.ParseException _ -> harvest db ws
            UnicodeException _ -> harvest db ws
            CloseRequest _ _ -> do 
                sendClose ws ("u said" :: T.Text)
                -- harvest db ws
    rec = do 
        mdown <- receiveData ws 
        case decode mdown of 
            Just town -> goDown town
            Nothing -> print mdown >> print "decode failed?" 
        where
        goDown down = case down of 
            See _ e@(Event _ _ (Content{kind})) -> case kind of 
                1 -> do 
                    trust <- verifyE e 
                    when trust (mask_ $ insertEv db e)  
                0 -> do 
                    trust <- verifyE e 
                    when trust (insertPl db e)
                _ -> print "?"
            Live _ -> print "--------live"
            Notice note -> print $ "note:" <> note 


extractURI :: URI -> Maybe (String, Word, String)
extractURI uri = do 
    a <- case auth of
        Right a -> Just a 
        _       -> Nothing 
    let host = T.unpack . unRText $ authHost a
    
    defaultport <- case unRText <$> uriScheme uri of
        (Just "wss") -> Just 443
        _ -> Just 80

    let port = maybe defaultport id $ authPort a
    let path = T.unpack $ maybe "/" joinpath $ uriPath uri
    pure (host, port, path)
    where 
    auth = uriAuthority uri
    joinpath (trailingSlash, rx) = if not trailingSlash 
        then joined `T.append` "/"
        else joined 
        where joined = P.foldl T.append "" $ 
                       fmap (flip T.append "/" . unRText) rx  

subscribe :: Text -> WS.Connection -> [Filter] -> IO ()
subscribe a conn = WS.sendTextData conn . encode . Subscribe a

liveF :: Integer -> Filter 
liveF sec = emptyF { 
      sinceF = Just $ Since $ sec 
    , kindsF = Just $ Kinds [0, 1] 
    , limitF = Just $ Limit 0 
    } 
