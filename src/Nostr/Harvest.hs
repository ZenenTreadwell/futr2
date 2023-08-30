module Nostr.Harvest where 

import Prelude as P
import Control.Exception as E
import Network.WebSockets as WS
import Wuss
import Database.SQLite.Simple as SQL
import Text.URI --(URI)
import Data.Text as T
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Aeson
import Control.Monad
import Nostr.Event
import Nostr.Wire 
import Nostr.Filter 
import Nostr.Beam

startCli :: SQL.Connection -> URI -> IO () 
startCli db uri =  
    case unRText . fromJust . uriScheme $ uri of 
        "wss" -> runSecureClient host (fromIntegral port) path ws 
        "ws"  -> WS.runClient host (fromIntegral port) path ws
        _ -> pure ()
    where 
    Just (host, port, path) = extractURI uri
    ws conn = do
        sec :: Integer <- round <$> getPOSIXTime
        subscribe "a" conn [liveF sec]             
        harvest db conn     

subscribe :: Text -> WS.Connection -> [Filter] -> IO ()
subscribe a conn fx = WS.sendTextData conn . encode $ Subscribe a fx

liveF :: Integer -> Filter 
liveF sec = emptyF { 
      sinceF = Just $ Since (sec + 11)
    , kindsF = Just $ Kinds [0, 1] 
    , limitF = Just $ Limit 0 } 



harvest :: SQL.Connection -> ClientApp () 
harvest db ws = catch (forever rec) \z -> do 
    print z 
    case z :: ConnectionException of  
        ConnectionClosed -> pure ()
        WS.ParseException s -> harvest db ws
        UnicodeException s -> harvest db ws
        CloseRequest w16 bs -> sendClose ws ("u said so" :: T.Text)
    where 
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
                    print . content . con $ e
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
    let port = maybe 443 id $ authPort a
    let path = T.unpack $ maybe "/" joinpath $ uriPath uri
    pure (host, port, path)
    where 
    auth = uriAuthority uri
    joinpath (trailingSlash, rx) = if not trailingSlash 
        then joined `T.append` "/"
        else joined 
        where joined = P.foldl T.append "" $ 
                       fmap (flip T.append "/" . unRText) rx  
