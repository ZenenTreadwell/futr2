
module Nostr where 

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- c (append, Text, unpack, pack)
import Data.Aeson as J
import GHC.Generics
import Data.Maybe

import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.Schnorr(
    signMsgSchnorr, verifyMsgSchnorr, msg, xOnlyPubKey, schnorrSig
    , KeyPair , keypair, SchnorrSig,  getSchnorrSig, deriveXOnlyPubKey, 
    getXOnlyPubKey)

isValid :: Event -> Bool 
isValid (Event i s e) = 
    let p = xOnlyPubKey . Hex.decodeLenient . pubkey $ e
        s' = schnorrSig . Hex.decodeLenient $ s
        m = msg . Hex.decodeLenient $ i
    in maybe False id $ verifyMsgSchnorr <$> p  <*> s'  <*> m

signEv :: KeyPair -> Ev -> Maybe Event 
signEv k e = Event <$> (Just i) <*> s' <*> (Just e)
    where 
    i = eventId e
    s = signMsgSchnorr <$> (Just k) <*> (msg . Hex.decodeLenient $ i)
    s' = Hex.encode . getSchnorrSig <$> s

data Event = Event {
      eid :: ByteString 
    , sig :: ByteString
    , eve :: Ev
    } deriving (Eq, Show)

data Ev = Ev {
      pubkey     :: ByteString
    , created_at :: Integer -- seconds
    , kind       :: Kind
    , tags       :: Value -- [Tag]
    , content    :: Text
    } deriving (Eq, Show, Generic)
instance ToJSON Ev
instance FromJSON Ev

eventId :: Ev -> ByteString 
eventId Ev{..} = Hex.encode . SHA256.hash . BS.toStrict . J.encode $ 
    [ Number 0
    , toJSON pubkey
    , Number $ fromIntegral created_at
    , Number $ fromIntegral kind
    , toJSON tags
    , String content
    ]

-- wire 
instance ToJSON Event where 
    toJSON (Event i s (Ev p c k t n)) = object [
          "id"         .= toJSON i
        , "pubkey"     .= toJSON p
        , "created_at" .= c
        , "kind"       .= k
        , "tags"       .= t 
        , "content"    .= n
        , "sig"        .= toJSON s      
        ]  

instance FromJSON Event where 
    parseJSON (Object o) =  
        let 
        i = o .: "id"
        p = o .: "pubkey"
        c = o .: "created_at"
        k = o .: "kind"
        t = o .: "tags"
        n = o .: "content"
        s = o .: "sig"
        ev = Ev <$> p 
                <*> c 
                <*> k
                <*> t 
                <*> n
        in Event <$> i 
                 <*> s
                 <*> ev
    
instance ToJSON ByteString where 
  toJSON s = String . decodeUtf8 $ s

instance FromJSON ByteString where 
  parseJSON (String s) = pure . encodeUtf8 $ s  

type Kind = Int
type Tag = Value
data Relay
data Filter
type SubId = Text
data Sub 
data NostrCli
    = CreateEvent Event
    | Subscribe SubId [Filter]
    | Close SubId 
    
data NostrServ
    = NewEvent SubId Event
    | Eose SubId
    | Notice Text
