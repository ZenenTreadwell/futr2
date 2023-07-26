module Nostr where 

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.Text (append, Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson
import qualified Crypto.Hash.SHA256 as SHA256
import GHC.Generics
import Data.Maybe 

import Crypto.Schnorr(verifyMsgSchnorr, msg, xOnlyPubKey, schnorrSig)

isValid :: Event -> Bool 
isValid (Event i s e) = 
    let p = xOnlyPubKey . Hex.decodeLenient . pubkey $ e
        s' = schnorrSig . Hex.decodeLenient $ s
        m = msg . Hex.decodeLenient $ i
    in maybe False id $ verifyMsgSchnorr <$> p  <*> s'  <*> m


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

instance ToJSON ByteString where 
  toJSON s = String . decodeUtf8 $ s

instance FromJSON ByteString where 
  parseJSON (String s) = pure . encodeUtf8 $ s  

eventId :: Ev -> ByteString 
eventId Ev{..} = Hex.encode . SHA256.hash . BS.toStrict . encode $ 
    [ Number 0
    , toJSON pubkey
    , Number $ fromIntegral created_at
    , Number $ fromIntegral kind
    , toJSON tags
    , String content
    ]

signEv :: Ev -> Event 
signEv e = Event i s e
    where 
    i = eventId e
    s = schnorr e

schnorr = undefined 

createEv = undefined

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

    
encodeHex :: ByteString -> Text  
encodeHex = decodeUtf8 . Hex.encode 
