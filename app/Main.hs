
module Main (main) where

import Text.URI --(URI)
import qualified Text.URI.QQ as QQ
import qualified Data.ByteString.Base16 as Hex
import Wuss
import Control.Exception as E
import qualified Data.ByteString.Lazy as LB
import Network.WebSockets as WS
import Control.Monad
import Data.Maybe
import Data.Aeson
import Control.Concurrent
import Database.SQLite.Simple as SQL
import Nostr.Event
import Nostr.Wire 
import Nostr.Filter 
import Nostr.Beam
import Data.Time.Clock.POSIX
import qualified Data.Text as T 
import Data.Either

import Control.Monad.Reader
import Control.Monad.State

-- import Nostr.Event
defaultRelay :: [URI] --_ -- m [URI] 
defaultRelay =  
    [ 
      -- [QQ.uri|ws://127.0.0.1:9481|]  
      [QQ.uri|wss://nos.lol|]
    , [QQ.uri|wss://relay.nostr.info|]
    , [QQ.uri|wss://relay.snort.social|]
    , [QQ.uri|wss://nostr-pub.wellorder.net|]
    , [QQ.uri|wss://nostr.oxtr.dev|]
    , [QQ.uri|wss://brb.io|]
    , [QQ.uri|wss://nostr-pub.semisol.dev|]
    , [QQ.uri|wss://nostr.zebedee.cloud|]
    , [QQ.uri|wss://relay.stoner.com|]
    , [QQ.uri|wss://relay.nostr.bg|]
    , [QQ.uri|wss://nostr-relay.untethr.me|]
    , [QQ.uri|wss://nostr.wine|]
    , [QQ.uri|wss://nostr.sandwich.farm|]
    , [QQ.uri|wss://nostr.rocks|] 
    , [QQ.uri|wss://relay.nostr.com.au|]
    , [QQ.uri|wss://nostrja-kari.heguro.com|]
    , [QQ.uri|wss://nostrja-kari-nip50.heguro.com|]
    ]

zippy :: [a] -> [(URI, a)]
zippy = zip defaultRelay -- startCli :: MonadIO m => URI -> ClientApp a -> m a 


startCli :: SQL.Connection -> URI -> IO () 
startCli db uri =  
    case unRText . fromJust . uriScheme $ uri of 
        "wss" -> runSecureClient host (fromIntegral port) path ws 
        "ws"  -> WS.runClient host (fromIntegral port) path ws
        _ -> pure ()
    where 
    Just (host, port, path) = extractURI uri
    ws conn = do
        print "client"
        sec :: Integer <- round <$> getPOSIXTime
        
        WS.sendTextData conn $ encode $ Subscribe "a" $ [
              emptyF { sinceF = Just $ Since sec
                     , kindsF = Just $ Kinds [0, 1] 
                     , limitF = Just $ Limit 0 } 
            ]
             
        catch (forever harvest) \z -> do 
            print z 
            print uri
            case z of  
                ConnectionClosed -> pure ()
                WS.ParseException s -> forever harvest 
                UnicodeException s -> forever harvest 
                CloseRequest w16 bs -> sendClose conn ("u said so" :: T.Text)
        where 

        harvest = fromJust . decode <$> receiveData conn >>= \case
            See _ e@(Event _ _ (Content{kind})) -> case kind of 
                1 -> do 
                    trust <- verifyE e 
                    when trust (mask_ $ insertEv db e)  
                    print "-----------"
                    print . render $ uri
                    print . content . con $ e
                0 -> do 
                    trust <- verifyE e 
                    when trust (insertPl db e)
                    print (trust, "insertPl ------")
                _ -> print "?"
            Live _ -> print "--------live"
            Notice note -> print $ "note:" <> note 
            -- Nothing -> pure () 

-- extractURI :: URI -> Maybe (String, _ , String) 
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
        where joined = foldl T.append "" $ 
                       fmap (flip T.append "/" . unRText) rx  

main :: IO ()
main = do 
    o <- open "./futr.sqlite"
    createDb o
    
    flip mapM defaultRelay $ \d -> forkIO $ startCli o d  

    threadDelay maxBound
    pure ()
    
-- killing z o = case z of 
--     -- ConnectionClosed -> myThreadId >>= killThread 

--     _ -> do 
--         print . show $ o
--         myThreadId >>= killThread                    

-- type Listen = IO (Either WS.ConnectionException LB.ByteString)

-- wsr :: SQL.Connection -> ClientApp ()
-- wsr db ws = forever do
--     print "server"
--     eo <- E.try . WS.receiveData $ ws :: Listen
--     case decode <$> eo of 
--         Right (Just d) -> case d of 
--             Subscribe s fx -> print "sub"  
--             Submit e -> do 
--                 trust <- verifyE e 
--                 when trust (mask_ $ insertEv db e)  
--             End s -> pure () --myThreadId >>= killThread 
--         Right Nothing -> print . (<> " - nothing") . show $ eo
--         Left z -> killing z eo

--     -- threadDelay 3000000  
--     -- kp <- genKeyPair 
--     -- pu <- exportPub kp
--     -- e <- signE kp $ Content 1 [
          
--     --       PTag pu Nothing
--     --     , 
--     --       ETag evref Nothing Nothing 

--     --     ] "test futr3 " sec  
--     -- sendE conn e
--     threadDelay maxBound

sendE :: WS.Connection -> Event -> IO ()
sendE c = WS.sendBinaryData c . encode . Submit 

evref = Hex32 $ Hex.decodeLenient "3da979448d9ba263864c4d6f14984c423a3838364ec255f03c7904b1ae77f206"




