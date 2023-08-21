{-# LANGUAGE 
    QuasiQuotes
  , TemplateHaskell
  #-}


module Main (main) where

-- modern-uri                          
import Text.URI --(URI)
-- import Control.Lensi
-- import Text.URI.Lens

-- import qualified Data.ByteString.Base16 as Hex 
-- import qualified Data.Base16.Types as Hex
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (append, Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- import Text.URI.Lens
import qualified Text.URI.QQ as QQ
--
import Wuss

import Control.Exception as E
import qualified Data.ByteString.Lazy as LB
-- import qualified Data.Map as Map

-- ?
import qualified Network.Connection as Connection
import Secp256k1.Internal 
import qualified Crypto.Hash.SHA256 as SHA256


import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.ECC.ECDSA (PublicKey, PrivateKey, Signature, sign, verify)

-- websockets
import Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream

import Control.Monad.IO.Class
import Data.Maybe
import Data.Either
import Control.Monad
import Data.Aeson
import Control.Concurrent

import Nostr.Event
import Nostr.Wire 
import Nostr.Filter 

import Data.Time.Clock.POSIX

-- import Nostr.Event
defaultRelay :: [URI] 
defaultRelay = 
    [ 
      [QQ.uri|ws://127.0.0.1:9481|]  
    --   [QQ.uri|wss://relay.kronkltd.net|]
    -- , [QQ.uri|wss://nostr-relay.untethr.me|]
    , [QQ.uri|wss://nostr.wine|] 
    -- , [QQ.uri|wss://nostr.sandwich.farm|]
    -- , [QQ.uri|wss://nostr.rocks|] 
    -- , [QQ.uri|wss://relay.nostr.bg|]
    ]
zippy = zip defaultRelay -- startCli :: MonadIO m => URI -> ClientApp a -> m a 

startCli uri app = forkIO $ do  
    print (host, port, path)
    case unRText . fromJust . uriScheme $ uri of 
        "wss" -> runSecureClient host (fromIntegral port) path app 
        "ws"  -> WS.runClient host (fromIntegral port) path app
        _ -> pure ()
    where 
    Just (host, port, path) = extractURI uri

-- extractURI :: URI -> Maybe (String, _ , String) 
extractURI uri = do 
    a <- case auth of
        Right a -> Just a 
        _       -> Nothing 
    let host = unpack . unRText $ authHost a
    let port = maybe 443 id $ authPort a
    let path = unpack $ maybe "/" joinpath $ uriPath uri
    pure (host, port, path)
    where 
    auth = uriAuthority uri
    joinpath (trailingSlash, rx) = if not trailingSlash 
        then joined `append` "/"
        else joined 
        where joined = foldl append "" $ 
                       fmap (flip append "/" . unRText) rx  

main :: IO ()
main = do 
    forkIO $ runServer "127.0.0.1" 9481 \p -> acceptRequest p >>= wsr 
    (zippy -> clientThreads) <- flip mapM defaultRelay $ \d -> startCli d ws
    threadDelay maxBound
    pure ()

wsr :: ClientApp () 
wsr conn = do 
    print "wsr"
    void . forkIO . forever $ runRelay 
    where 
    runRelay = do
        eo <- E.try . receiveData $ conn 
                    :: IO (Either WS.ConnectionException LB.ByteString)
        case decode <$> eo of 
            Right (Just d) -> case d of 
                Subscribe s fx -> print "!!!!!!!!!!! subscribe !!!!!!!!!!" 
                Submit e -> print . content . con $ e  
                End s -> print "!!!!!!!! end !!!!!!!!!!!" 
            Right Nothing -> print "--------up incomplete"
            Left z -> do 
                print . ("----left UP " <>) . show $ z
                myThreadId >>= killThread 

ws :: ClientApp ()
ws conn = do
    print "ws"
    void . forkIO . forever $ do
        eo <- E.try . receiveData $ conn 
                    :: IO (Either WS.ConnectionException LB.ByteString)
        case decode <$> eo of 
            Right (Just d) -> case d of 
                See subid e -> do 
                    print . content . con $ e
                    trust <- verifyE e
                    print trust
                Live subid -> print "--------live"
                Notice note -> print $ "note:" <> note 
            Right Nothing -> print "--------down incomplete"
            Left z -> do 
                print . ("----left DOWN " <>) . show $ z
                myThreadId >>= killThread 
    sec :: Integer <- round <$> getPOSIXTime
    WS.sendBinaryData conn $ encode $ Subscribe "a" $ [
          Filter [Since sec, Kinds [1]] Nothing
        -- , Filter [Kinds [0], Authors ["460c25e682fd"]] Nothing
        ] 
    threadDelay maxBound
    -- sendClose connection (pack "Bye!")