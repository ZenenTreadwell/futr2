
module Nostr.Event where 

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Data.ByteString.Base16 as Hex

import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson as J
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Either
import Data.Text (Text)
import Data.Maybe
import GHC.Generics 
import Data.Time.Clock.POSIX
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array as F
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

import qualified Crypto.Hash.SHA256 as SHA256
-- import Crypto.Schnorr
-- import Crypto.Secp256k1
-- import Crypto.Secp256k1.Internal
-- import Crypto.Secp256k1.Prim

import System.IO.Unsafe
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as Char8
-- import Crypto.Schnorr

import Crypto.Schnorr.Internal

isValid :: Event -> Bool 
isValid (Event{..}) = 
    let p = xOnlyPubKey . un32 . pubkey $ con
        s' = schnorrSig . un64 $ sig
        m = msg . un32 $ eid
    in maybe False id $ verifyMsgSchnorr <$> p  <*> s'  <*> m

getPtr :: ByteString -> IO (Ptr x, CSize)  
getPtr bs = 
    BU.unsafeUseAsCStringLen bs \(p, l) -> pure (castPtr p, fromIntegral l) 

getSignPub :: Hex32 -> IO Hex64
getSignPub (Hex32 bs) = do 
    pub64 <- mallocBytes 64
    (pub32, 32) <- getPtr bs
    ret <- schnorrXOnlyPubKeyParse ctx pub64 pub32
    case ret of
        1 -> Hex64 <$> BU.unsafePackMallocCStringLen (castPtr pub64, 64) 
        0 -> undefined 

verifyE :: Event -> Bool 
verifyE Event{..}  
    | idE con == eid = unsafePerformIO $ do 
        signPub <- getSignPub . pubkey $ con
        (msg', 32) <- getPtr (un32 eid) -- \(msg', 32) ->  
        (sig', 64) <- getPtr (un64 sig) 
        (pub', 64) <- getPtr (un64 signPub)
        (== 1) <$> schnorrSignatureVerify ctx sig' msg' 32 pub' 
    | otherwise = False 

signE :: KeyPair -> Content -> Maybe Event
signE kp c@(Content{..}) = 
    let i' = idE c
        m' = msg . un32 $ i'   
        s' = signMsgSchnorr kp <$> m' 
    in Event i' <$> (Hex64 . getSchnorrSig <$> s') <*> (Just c)     


-- signE :: Hex32 -> (Hex32 -> Integer -> Content) -> IO (Maybe Event) 
-- signE kp partC  =
--     let 

--     sec :: Integer <- round <$> getPOSIXTime
--     let pubkey = Hex32 . getXOnlyPubKey . deriveXOnlyPubKey $ kp  
--         con = partC pubkey sec
--         i = idE con 
--     print . toJSON $ pubkey
--     print . toJSON $ i
--     print "pubkey"
--     Just sig <- pure $ Hex64 . getSchnorrSig . signMsgSchnorr kp 
--          <$> (msg . un32 $ i)
--     pure . pure $ Event i sig con

idE :: Content -> Hex32
idE Content{..} = Hex32 
    . Hex.decodeLenient 
    . Hex.encode 
    . SHA256.hash 
    . BS.toStrict 
    . J.encode $ 
        [ Number 0
        , toJSON pubkey
        , Number $ fromIntegral created_at
        , Number $ fromIntegral kind
        , toJSON tags
        , String content
        ]

createC :: KeyPair -> Int -> [Tag] -> Text -> IO Content
createC kp kind' tag' content' = do 
    sec :: Integer <- round <$> getPOSIXTime
    let pk = Hex32 . schnorrPort $ kp
    pure $ Content kind' tag' content' pk sec

data Event = Event {
      eid :: Hex32
    , sig :: Hex64
    , con :: Content
    } deriving (Eq, Show)

data Content = Content {
      kind       :: Int
    , tags       :: [Tag]
    , content    :: Text
    , pubkey     :: Hex32
    , created_at :: Integer
    } deriving (Eq, Show, Generic)
instance ToJSON Content
instance FromJSON Content

instance ToJSON Event where 
    toJSON (Event i s (Content{..})) = object [
          "id"         .= i
        , "sig"        .= s      
        , "pubkey"     .= pubkey
        , "created_at" .= created_at
        , "kind"       .= kind
        , "tags"       .= tags 
        , "content"    .= content
        ]  

instance FromJSON Event where 
    parseJSON = withObject "event" \o ->  
        Event <$> o .: "id" 
              <*> o .: "sig"
              <*> (Content 
                  <$> o .: "kind"
                  <*> o .: "tags" 
                  <*> o .: "content"
                  <*> o .: "pubkey" 
                  <*> o .: "created_at" 
                  )
data Tag = 
      ETag Hex32 (Maybe Text) (Maybe Marker)
    | PTag Hex32 (Maybe Text)
    | Tag  Array
    deriving (Eq, Show, Generic)
data Marker = Reply | Root | Mention
    deriving (Eq, Show, Generic)
    
instance FromJSON Tag where 
    parseJSON = J.withArray "tag" \a -> do 
        let tag = a V.! 0
            evId = parseJSON (a V.! 1)
            rel = sequenceA $ parseJSON <$> a V.!? 2
            mar = sequenceA $ parseJSON <$> a V.!? 3
        case tag of 
            String "e" -> ETag <$> evId <*> rel <*> mar 
            String "p" -> PTag <$> evId <*> rel
            _ -> pure $ Tag a
instance ToJSON Tag where 
    toJSON (ETag i mr mm) = toJSON $ case (mr, mm) of 
        (Just r, Just m) -> ee <> [toJSON r, toJSON m]
        (Nothing, Just m) -> ee <> [String "", toJSON m]
        (Just r, Nothing) -> ee <> [toJSON r]
        (Nothing, Nothing) -> ee
        where 
        ee = [String "e", toJSON i]
    toJSON (PTag i mr) = toJSON case mr of 
        Just r -> [String "p", toJSON i, toJSON r]
        Nothing -> [String "p", toJSON i]
    toJSON (Tag a) = toJSON a                       
  
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
    
newtype Hex64 = Hex64 { un64 :: ByteString } deriving (Eq, Show)
newtype Hex32 = Hex32 { un32 :: ByteString } deriving (Eq, Show)

instance ToJSON Hex64 where
  toJSON (Hex64 bs) = toHex bs

instance ToJSON Hex32 where
  toJSON (Hex32 bs) = toHex bs 

instance FromJSON Hex64 where
  parseJSON v = parseHex v >>= hex64 
    where 
    hex64 bs | BS.length bs == 64 = pure $ Hex64 bs
             | otherwise          = fail "length not 64"

instance FromJSON Hex32 where
  parseJSON v = parseHex v >>= hex32 
    where 
    hex32 bs | BS.length bs == 32 = pure $ Hex32 bs
             | otherwise          = fail "length not 32"

toHex :: ByteString -> Value
toHex = toJSON . decodeUtf8 . Hex.encode 

parseHex :: Value -> Parser ByteString
parseHex = withText "HexByteString" $ \txt -> do
    case Hex.decode . encodeUtf8 $ txt of
      Left err -> fail err
      Right bs -> return bs

