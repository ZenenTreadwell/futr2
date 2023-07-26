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
import qualified Data.ByteString.Base16 as Hex
import Data.Text (append, Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- import Text.URI.Lens
import qualified Text.URI.QQ as QQ
--
import Wuss

import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as LB
-- import qualified Data.Map as Map

-- ?
import qualified Network.Connection as Connection
import Crypto.Schnorr (KeyPair, SchnorrSig, XOnlyPubKey,
                       decodeHex, exportSchnorrSig, exportXOnlyPubKey,
                       msg, signMsgSchnorr, verifyMsgSchnorr)
import qualified Crypto.Hash.SHA256 as SHA256


import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.ECC.ECDSA (PublicKey, PrivateKey, Signature, sign, verify)

-- websockets
import Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream

import Data.Maybe
import Control.Monad
import Data.Aeson
import Control.Concurrent
import Nostr
curve :: Curve 
curve = getCurveByName SEC_p256k1
-- m (PublicKey, PrivateKey)	

keypair :: IO (PublicKey, PrivateKey) 
keypair = generate curve >>= pure  

-- import Nostr.Event
defaultRelay :: [URI] 
defaultRelay = 
    [ [QQ.uri|wss://nostr.wine|] 
    , [QQ.uri|wss://nostr.rocks|] 
    , [QQ.uri|wss://relay.nostr.bg|] 
    , [QQ.uri|wss://nostr-relay.untethr.me|]
    , [QQ.uri|wss://relay.kronkltd.net|]
    ]
    


main :: IO ()
main = undefined  

