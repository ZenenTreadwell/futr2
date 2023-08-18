
module Nostr.Event where 

import qualified Data.ByteString as BS
import Data.ByteString.Base16 as Hex

import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson as J
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Text (Text)
import GHC.Generics 
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Crypto.Random.DRBG
import qualified Crypto.Hash.SHA256 as SHA256
import System.IO.Unsafe
import Secp256k1.Internal

parsePub :: Hex32 -> IO Hex64
parsePub (Hex32 bs) = do 
    pub64 <- mallocBytes 64
    (pub32, 32) <- getPtr bs
    ret <- schnorrXOnlyPubKeyParse ctx pub64 pub32
    case ret of
        1 -> Hex64 <$> packPtr (pub64, 64) 
        _ -> undefined -- XXX how sometimes?

genKeyPair :: IO Hex96
genKeyPair = do 
    gen <- newGenIO :: IO CtrDRBG
    let Right (bs, _) = genBytes 32 gen
    (salt, 32) <- getPtr bs
    keypair <- mallocBytes 96
    ret <- keyPairCreate ctx keypair salt
    case ret of 
        1 -> Hex96 <$> packPtr (keypair, 96)
        _ -> undefined 

exportKeyPair :: Hex96 -> IO Hex32 
exportKeyPair (Hex96 bs) = do 
    (priv, 96) <- getPtr bs
    pub64 <- mallocBytes 64
    _ <- keyPairXOnlyPubKey ctx pub64 nullPtr priv
    pub <- mallocBytes 32
    _ <- schnorrPubKeySerialize ctx pub (castPtr pub64)
    Hex32 <$> packPtr (pub, 32)

verifyE :: Event -> Bool 
verifyE Event{..}  
    | idE con == eid = unsafePerformIO $ do 
        signPub <- parsePub . pubkey $ con
        (msg', 32) <- getPtr (un32 eid) -- \(msg', 32) ->  
        (sig', 64) <- getPtr (un64 sig) 
        (pub', 64) <- getPtr (un64 signPub)
        (== 1) <$> schnorrSignatureVerify ctx sig' msg' 32 pub' 
    | otherwise = False 

signE :: Hex96 -> Content -> Event
signE kp c@(Content{..}) = 
  let eid = idE c
  in unsafePerformIO do
    (priv, 96) <- getPtr (un96 kp)
    sig <- mallocBytes 64
    (msg, 32) <- getPtr . un32 $ eid
    gen <- newGenIO :: IO CtrDRBG
    let Right (bs, _) = genBytes 32 gen
    (salt, 32) <- getPtr bs
    ret <- schnorrSign ctx sig msg priv salt
    case ret of 
        1 -> do 
            sigBS <- Hex64 <$> packPtr (sig, 64)
            let newE = Event eid sigBS c
            pure if verifyE newE 
                then newE
                else signE kp c
        _ -> pure $ signE kp c
             
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
            rel = traverse parseJSON $ a V.!? 2
            mar = traverse parseJSON $ a V.!? 3
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

