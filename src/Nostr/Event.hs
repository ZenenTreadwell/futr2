
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

packPtr :: (Ptr x, CSize) -> IO ByteString 
packPtr (p, l) = BU.unsafePackMallocCStringLen (castPtr p, fromIntegral l) 

getSignPub :: Hex32 -> IO Hex64
getSignPub (Hex32 bs) = do 
    pub64 <- mallocBytes 64
    (pub32, 32) <- getPtr bs
    ret <- schnorrXOnlyPubKeyParse ctx pub64 pub32
    case ret of
        1 -> Hex64 <$> packPtr (pub64, 64) 
        0 -> undefined 

exportKeyPair :: Hex96 -> IO Hex32 
exportKeyPair (Hex96 bs) = do 
    (priv, 96) <- getPtr bs
    pub <- mallocBytes 32
    schnorrPubKeySerialize ctx pub priv
    Hex32 <$> packPtr (pub, 32)

verifyE :: Event -> Bool 
verifyE Event{..}  
    | idE con == eid = unsafePerformIO $ do 
        signPub <- getSignPub . pubkey $ con
        (msg', 32) <- getPtr (un32 eid) -- \(msg', 32) ->  
        (sig', 64) <- getPtr (un64 sig) 
        (pub', 64) <- getPtr (un64 signPub)
        (== 1) <$> schnorrSignatureVerify ctx sig' msg' 32 pub' 
    | otherwise = False 

-- signE :: KeyPair -> Content -> 

signE :: Hex96 -> Content -> Event
signE kp c@(Content{..}) = 
  let eid = idE c
  in unsafePerformIO do
    (priv, 96) <- getPtr (un96 kp)
    sig <- mallocBytes 64
    (msg, 32) <- getPtr . un32 $ eid
    ret <- schnorrSign ctx sig msg priv nullPtr
    case ret of 
        1 -> do 
            sigBS <- packPtr (sig, 64)
            pure $ Event eid (Hex64 sigBS) c
        _ -> undefined 
             
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

createC :: Hex96 -> Int -> [Tag] -> Text -> IO Content
createC kp kind' tag' content' = do 
    (priv, 96) <- getPtr (un96 kp)
    pub32 <- mallocBytes 32
    schnorrPubKeySerialize ctx pub32 priv
    serialPub <- Hex32 <$> packPtr (pub32, 32)
    sec :: Integer <- round <$> getPOSIXTime
    pure $ Content kind' tag' content' serialPub sec

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
    
newtype Hex96 = Hex96 { un96 :: ByteString } deriving (Eq, Show)
newtype Hex64 = Hex64 { un64 :: ByteString } deriving (Eq, Show)
newtype Hex32 = Hex32 { un32 :: ByteString } deriving (Eq, Show)

instance ToJSON Hex96 where
  toJSON (Hex96 bs) = toHex bs

instance ToJSON Hex64 where
  toJSON (Hex64 bs) = toHex bs

instance ToJSON Hex32 where
  toJSON (Hex32 bs) = toHex bs 

instance FromJSON Hex96 where
  parseJSON v = parseHex v >>= (fixBS 96 Hex96)
            
instance FromJSON Hex64 where
  parseJSON v = parseHex v >>= (fixBS 64 Hex64) 

instance FromJSON Hex32 where
  parseJSON v = parseHex v >>= (fixBS 32 Hex32) 

fixBS l c bs
    | BS.length bs == l = pure . c $ bs
    | otherwise = fail $ "incorrect length " <> show l

toHex :: ByteString -> Value
toHex = toJSON . decodeUtf8 . Hex.encode 

parseHex :: Value -> Parser ByteString
parseHex = withText "HexByteString" $ \txt -> do
    case Hex.decode . encodeUtf8 $ txt of
      Left err -> fail err
      Right bs -> return bs

