module Nostr.Keys where 

import Control.Monad
import Data.ByteString.Base16 as Hex
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson as J
import Data.Aeson.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Secp256k1.Internal 
import Data.Text (Text)
import Codec.Binary.Bech32 as BECH
import System.Entropy

npub (Hex32 b) = bechin "npub" b  

nsec :: Hex96 -> Text 
nsec (Hex96 b) = bechin "nsec" (BS.take 32 b)

bechin :: Text -> ByteString -> Text
bechin prefix bs =
  let Right pf = humanReadablePartFromText prefix
  in case BECH.encode pf . dataPartFromBytes $ bs of
      Right t -> t
      Left _ -> error "invalid npub"

xnpub :: Text -> Maybe Hex32
xnpub t = case BECH.decode t of 
    Right (_, d) -> case dataPartToBytes d of 
        Just s -> J.decode . J.encode $ Hex32 s 
        _ -> Nothing
    Left _ -> Nothing
 
newtype Hex96 = Hex96 { un96 :: ByteString } deriving (Eq, Show, Ord)
newtype Hex64 = Hex64 { un64 :: ByteString } deriving (Eq, Show, Ord)
newtype Hex32 = Hex32 { un32 :: ByteString } deriving (Eq, Show, Ord)

instance ToJSON Hex96 where
  toJSON (Hex96 bs) = toHex bs

instance ToJSON Hex64 where
  toJSON (Hex64 bs) = toHex bs

instance ToJSON Hex32 where
  toJSON (Hex32 bs) = toHex bs 

instance FromJSON Hex96 where
  parseJSON v = parseHex v >>= fixBS 96 Hex96
            
instance FromJSON Hex64 where
  parseJSON v = parseHex v >>= fixBS 64 Hex64 

instance FromJSON Hex32 where
  parseJSON v = parseHex v >>= fixBS 32 Hex32 

fixBS :: Int -> (ByteString -> b) -> ByteString -> Parser b
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

genKeyPair :: IO Hex96
genKeyPair = do 
    x <- Hex32 <$> getEntropy 32
    expandPriv x

expandPriv :: Hex32 -> IO Hex96 
expandPriv (Hex32 bs) = do
    (salt, 32) <- getPtr bs
    keypair <- mallocBytes 96
    ret <- keyPairCreate ctx keypair salt
    case ret of 
        1 -> Hex96 <$> packPtr (keypair, 96)
        _ -> undefined 

exportPub :: Hex96 -> IO Hex32 
exportPub (Hex96 bs) = do 
    (priv, 96) <- getPtr bs
    pub64 <- mallocBytes 64
    void $ keyPairXOnlyPubKey ctx pub64 nullPtr priv
    pub <- mallocBytes 32
    void $ schnorrPubKeySerialize ctx pub (castPtr pub64)
    Hex32 <$> packPtr (pub, 32)
    
parsePub :: Hex32 -> IO Hex64
parsePub (Hex32 bs) = do 
    pub64 <- mallocBytes 64
    (pub32, 32) <- getPtr bs
    ret <- schnorrXOnlyPubKeyParse ctx pub64 pub32
    case ret of
        1 -> Hex64 <$> packPtr (pub64, 64) 
        _ -> free pub64 >> error "parsePub error"

