
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
import Crypto.Schnorr
import Data.Vector as V

eventId :: Ev -> ByteString 
eventId Ev{..} = Hex.decodeLenient . Hex.encode . SHA256.hash . BS.toStrict . J.encode $ 
    [ Number 0
    , toJSON pubkey
    , Number $ fromIntegral created_at
    , Number $ fromIntegral kind
    , toJSON tags
    , String content
    ]

isValid :: Event -> Maybe Bool 
isValid (Event i s e) = 
    let p = xOnlyPubKey . pubkey $ e
        s' = schnorrSig s
        m = msg i
    in verifyMsgSchnorr <$> p  <*> s'  <*> m

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
    parseJSON = withObject "event" \o ->  
        Event <$> o .: "id" 
              <*> o .: "sig"
              <*> (Ev <$> o .: "pubkey" 
                      <*> o .: "created_at" 
                      <*> o .: "kind"
                      <*> o .: "tags" 
                      <*> o .: "content")

instance ToJSON ByteString where
  toJSON bs = toJSON $ decodeUtf8 $ Hex.encode bs

instance FromJSON ByteString where
  parseJSON = withText "HexByteString" $ \txt -> do
    let hexStr = encodeUtf8 txt
    case Hex.decode hexStr of
      Left err -> fail err
      Right bs -> return bs

type Kind = Int
type Tag = Value
data Relay
-- data Filter
type Filter = Value
type SubId = Text
data Sub 
data Up
    = Submit Event
    | Subscribe SubId [Filter]
    | Close SubId 
    deriving (Generic)
data Down
    = See SubId Event
    | Last SubId
    | Notice Text

instance FromJSON Up where 
    parseJSON = withArray "up" \a -> 
        case V.head a of 
            "EVENT" -> Submit <$> parseJSON (V.last a)
            -- "REQ" -> fail "unimpl req"
            
instance ToJSON Up where 
    toJSON (Submit e) = toJSON [
          String "EVENT"
        , toJSON e
        ]
    toJSON (Subscribe s fx) = toJSON $ [
          String "REQ"
        , String s 
        ] <> fx

instance FromJSON Down where 
    parseJSON = withArray "down" \a -> 
        case (V.head a, V.head . V.tail $ a) of 
            ("EVENT", s) -> See <$> (parseJSON s) <*> parseJSON (V.last a)        

instance ToJSON Down where 
    toJSON (See s e) = toJSON [
          String "EVENT"
        , String s
        , toJSON e
        ]