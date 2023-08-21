{-# LANGUAGE 
    QuasiQuotes
  , TemplateHaskell
  #-}


module Main (main) where

-- modern-uri                          
import Text.URI -- (URI, render)

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
import Nostr.Relay
import Nostr.Filter 

import Data.Time.Clock.POSIX

curve :: Curve 
curve = getCurveByName SEC_p256k1
-- m (PublicKey, PrivateKey)	

keypair :: IO (PublicKey, PrivateKey) 
keypair = generate curve >>= pure  

-- import Nostr.Event
defaultRelay :: [URI] 
defaultRelay = 
    [ [QQ.uri|wss://relay.kronkltd.net|]
    , [QQ.uri|wss://nostr-relay.untethr.me|]
    , [QQ.uri|wss://nostr.wine|] 
    , [QQ.uri|wss://nostr.sandwich.farm|]
    , [QQ.uri|wss://nostr.rocks|] 
    , [QQ.uri|wss://relay.nostr.bg|] 
    ]

-- startCli :: MonadIO m => URI -> ClientApp a -> m a 
startCli uri app = 
    runSecureClient host (fromIntegral port) path app 
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
main = startCli (head . drop 2 $ defaultRelay) ws

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"
    -- putStrLn $ show connection 
    void . forkIO . forever $ do
        eo <- E.try . receiveData $ connection 
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
                print . ("----left " <>) . show $ z
                throw Deadlock
    sec :: Integer <- round <$> getPOSIXTime
    WS.sendBinaryData connection $ encode $ Subscribe "a" $ [
          Filter [Since sec, Kinds [1]] Nothing
        -- , Filter [Kinds [0], Authors ["460c25e682fd"]] Nothing
        ] 
    threadDelay maxBound
    -- sendClose connection (pack "Bye!")