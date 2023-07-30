
module Nostr where 

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- c (append, Text, unpack, pack)
import Data.Aeson as J
import GHC.Generics
import Data.Maybe
import Data.Either
import qualified "base16-bytestring" Data.ByteString.Base16 as Hex
import qualified "base16" Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.Schnorr(
    signMsgSchnorr, verifyMsgSchnorr, msg, xOnlyPubKey, schnorrSig
    , KeyPair , keypair, SchnorrSig,  getSchnorrSig, deriveXOnlyPubKey, 
    getXOnlyPubKey)

eventId :: Ev -> ByteString 
eventId Ev{..} = Hex.decodeLenient . Hex.encode . SHA256.hash . BS.toStrict . J.encode $ 
    [ Number 0
    , toJSON pubkey
    , Number $ fromIntegral created_at
    , Number $ fromIntegral kind
    , toJSON tags
    , String content
    ]

isValid :: Event -> Bool 
isValid (Event i s e) = 
    let p = xOnlyPubKey . pubkey $ e
        s' = schnorrSig s
        m = msg i
    in maybe False id $ verifyMsgSchnorr <$> p  <*> s'  <*> m

signEv :: KeyPair -> Ev -> Maybe Event 
signEv k e = Event <$> (Just i) <*> s' <*> (Just e)
    where 
    i = eventId e
    s = signMsgSchnorr <$> (Just k) <*> (msg i)
    s' = getSchnorrSig <$> s

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
  toJSON bs = toJSON $ decodeUtf8 $ Hex.encode bs

instance FromJSON ByteString where
  parseJSON = withText "HexByteString" $ \txt -> do
    let hexStr = encodeUtf8 txt
    case Hex.decode hexStr of
      Left err -> fail err
      Right bs -> return bs

    
-- instance ToJSON ByteString where 
--   toJSON =  

-- instance FromJSON ByteString where 
--   parseJSON (String s) = 

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
