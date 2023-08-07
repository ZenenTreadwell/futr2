
module Nostr where 

import Data.ByteString as BS
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson as J
import GHC.Generics
import Data.Maybe
import Data.Either
import Data.ByteString.Base16 as Hex
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.Schnorr
import Data.Vector as V

eventId :: Ev -> Hex32
eventId Ev{..} = Hex32 . Hex.decodeLenient . Hex.encode . SHA256.hash . BS.toStrict . J.encode $ 
    [ Number 0
    , toJSON pubkey
    , Number $ fromIntegral created_at
    , Number $ fromIntegral kind
    , toJSON tags
    , String content
    ]

isValid :: Event -> Maybe Bool 
isValid (Event i s e) = 
    let p = xOnlyPubKey . un32 . pubkey $ e
        s' = schnorrSig . un64 $ s
        m = msg . un32 $ i
    in verifyMsgSchnorr <$> p  <*> s'  <*> m

signEv :: KeyPair -> Ev -> Maybe Event 
signEv k e = do 
    i <- pure $ eventId e
    s <- getSchnorrSig . (signMsgSchnorr k) <$> (msg . un32 $ i)
    pure $ Event i (Hex64 s) e

data Event = Event {
      eid :: Hex32
    , sig :: Hex64
    , eve :: Ev
    } deriving (Eq, Show)

data Ev = Ev {
      pubkey     :: Hex32
    , created_at :: Integer -- seconds
    , kind       :: Kind
    , tags       :: [Tag]
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

type SubId = Text
type Relay = Text
type Kind = Int
-- type Tag = Value
data Tag = 
      ETag Hex32 (Maybe Relay) (Maybe Marker)
    | PTag Hex32 (Maybe Relay)
    | Tag  Array
    deriving (Eq, Show, Generic)
    

instance FromJSON Tag where 
    parseJSON = withArray "tag" \a -> do 
        let tag = (a V.! 0)
            evId = parseJSON (a V.! 1)
            rel = traverse id $ parseJSON <$> (a V.!? 2)
            mar = traverse id $ parseJSON <$> a V.!? 3
        case tag of 
            String "e" -> ETag <$> evId <*> rel <*> mar 
            String "p" -> PTag <$> evId <*> rel
            _ -> pure $ Tag a

instance ToJSON Tag where 
    toJSON (ETag i mr mm) = case (mr, mm) of 
        (Just r, Just m) -> toJSON $ [String "e", toJSON i, toJSON r, toJSON m]
        (Nothing, Just m) -> toJSON $ [String "e", toJSON i, String "", toJSON m]
        (Just r, Nothing) -> toJSON $ [String "e", toJSON i, toJSON r]
        (Nothing, Nothing) -> toJSON [String "e", toJSON i]
    toJSON (PTag i mr) = case mr of 
        Just r -> toJSON [String "p", toJSON i, toJSON r]
        Nothing -> toJSON [String "p", toJSON i]
    toJSON (Tag a) = toJSON a                       
  
data Marker = Reply | Root | Mention
              deriving (Eq, Show, Generic)

instance FromJSON Marker where 
    parseJSON = withText "marker" \case 
        "reply" -> pure Reply
        "root"  -> pure Root
        "mention" -> pure Mention 
        _ -> fail "invalid marker"

instance ToJSON Marker where 
    toJSON Reply = String "reply"
    toJSON Root = String "root"
    toJSON Mention = String "mention"
    

type Filter = Value

data Up
    = Submit Event
    | Subscribe SubId [Filter]
    | Close SubId 
    deriving (Generic)
data Down
    = See SubId Event
    | Live SubId
    | Notice Text

instance FromJSON Up where 
    parseJSON = withArray "up" \a -> 
        case V.head a of 
            "EVENT" -> Submit <$> parseJSON (V.last a)
            "REQ" -> Subscribe <$> parseJSON (a V.! 1) 
                               <*> pure (V.foldr (:) [] (V.drop 2 a)) 
            "CLOSE" -> Close <$> parseJSON (V.last a)
            _ -> fail "unimpl parseJSON"
            
instance ToJSON Up where 
    toJSON (Submit e) = toJSON [
          String "EVENT"
        , toJSON e
        ]
    toJSON (Subscribe s fx) = toJSON $ [
          String "REQ"
        , String s 
        ] <> fx
    toJSON (Close s) = toJSON [String "CLOSE", String s]

instance FromJSON Down where 
    parseJSON = withArray "down" \a -> 
        case (V.head a, V.head . V.tail $ a) of 
            ("EVENT", String s) -> See s <$> parseJSON (V.last a)        
            ("EOSE", String s) -> pure $ Live s
            ("NOTICE", String n) -> pure $ Notice n
            _ -> fail . show $ a
            

instance ToJSON Down where 
    toJSON (See s e) = toJSON [
          String "EVENT"
        , String s
        , toJSON e
        ]
    toJSON (Live s) = toJSON [String "EOSE", String s]
    toJSON (Notice n) = toJSON [String "NOTICE", String n]
    
    

newtype Hex64 = Hex64 { un64 :: ByteString } deriving (Eq, Show)
newtype Hex32 = Hex32 { un32 :: ByteString } deriving (Eq, Show)

instance ToJSON Hex64 where
  toJSON (Hex64 bs) = toHex bs
      -- \| BS.length bs == 64 = toHex bs
      -- | otherwise = fail "incorrect length"

instance ToJSON Hex32 where
  toJSON (Hex32 bs) = toHex bs 

instance FromJSON Hex64 where
  parseJSON v = parseHex v >>= hex64 
    where 
    hex64 bs | BS.length bs == 64 = pure $ Hex64 bs
             | otherwise          = fail "length not 64"

instance FromJSON Hex32 where
  parseJSON v = (parseHex v) >>= hex32 
    where 
    hex32 bs | BS.length bs == 32 = pure $ Hex32 bs
             | otherwise          = fail "length not 32"

toHex = toJSON . decodeUtf8 . Hex.encode 

parseHex = withText "HexByteString" $ \txt -> do
    let hexStr = encodeUtf8 txt
    case Hex.decode hexStr of
      Left err -> fail err
      Right bs -> return bs
