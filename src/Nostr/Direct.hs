module Nostr.Direct where 

import Prelude as P 
import Crypto.Cipher.Types
import Data.Text as T
import Crypto.Cipher.AES (AES256)
import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Crypto.Error
import System.Entropy 
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Secp256k1.Internal
import Nostr.Keys
import Nostr.Event
import Data.ByteArray.Encoding

encodeBase64' :: ByteString -> ByteString 
encodeBase64' = B64.encode 

encodeBase64 :: ByteString -> ByteString 
encodeBase64 = convertToBase Base64 

decodeBase64 :: ByteString -> ByteString  
decodeBase64 t = case convertFromBase Base64 t of 
    Right a -> a
    _ -> error "invalid base64"

type Msg = ByteString 
type Iv = ByteString 
type Shared = Hex32
data AesCtx = AesCtx AES256 (IV AES256) Iv
    
getShared :: Hex96 -> Hex32 -> IO Hex32 
getShared kp pu = do 
    se <- mallocBytes 32 
    (kp', 96) <- getPtr $ un96 kp 
    _ <- keyPairSec ctx se kp' 
    sh <- mallocBytes 32 
    ha <- hashPtr copyX
    (pu', 64) <- parsePub pu >>= getPtr . un64
    r <- ecdh ctx sh pu' se ha nullPtr  
    if r == 1 
        then Hex32 <$> packPtr (sh, 32)
        else error "hh"
   
encryptE :: Hex96 -> Hex32 -> Text -> IO Event
encryptE kp re msg = do  
    iv <- getEntropy 16
    sh <- getShared kp re
    let xx = createCtx sh iv
    m <- encryptMsg xx (encodeUtf8 msg)
    n <- round <$> getPOSIXTime
    let c = Content 4 [PTag re Nothing Nothing] m n 
    signE kp c

decryptE :: Hex96 -> Event -> IO (Maybe Text) 
decryptE kp (Event _ _ c) = do 
    case extract . content $ c of 
        Just (_, iv) -> do 
            sh <- getShared kp (pubkey c)
            let ccc = createCtx sh iv
            pure $ decryptMsg ccc (content c)
        Nothing -> pure Nothing 

createCtx :: Shared -> Iv -> AesCtx
createCtx sh iv = AesCtx xo ox iv
    where 
        xo :: AES256
        xo = case cipherInit . un32 $ sh :: CryptoFailable AES256 of 
            CryptoPassed a -> a 
            _ -> error "ff"
        ox :: IV AES256
        ox = case makeIV iv of 
            Just a -> a
            _ -> error "gg" 

encryptMsg :: AesCtx -> Msg -> IO Text 
encryptMsg (AesCtx xo ox iv) (pad -> m) = pure . decodeUtf8 $ fin 
    where     
    msg = encodeBase64 $ cbcEncrypt xo ox m
    fin = msg <> "?iv=" <> encodeBase64 iv

decryptMsg :: AesCtx -> Text -> Maybe Text  
decryptMsg (AesCtx xo ox _) t = do
    msg <- fst <$> extract t
    case decodeUtf8' . unpad  . cbcDecrypt xo ox $ msg of 
        Left _ -> Nothing
        Right m -> Just m
        
pad :: ByteString -> ByteString
pad m = m <> BS.replicate (16 - mod (BS.length m) 16) 10

unpad :: ByteString -> ByteString 
unpad = BS.reverse . BS.dropWhile (==10) . BS.reverse
    
extract :: Text -> Maybe (ByteString, ByteString) 
extract t = case T.splitOn "?iv=" t of 
    [ m, iv ] -> Just (d m, d iv)
    _ -> Nothing 
    where d = decodeBase64 . encodeUtf8 

prepare :: Hex32 -> ByteString -> IO AesCtx
prepare sh iv = pure . AesCtx xo ox $ iv
    where 
    xo :: AES256
    xo = case cipherInit . un32 $ sh :: CryptoFailable AES256 of 
        CryptoPassed a -> a 
        _ -> error "ff"
    ox :: IV AES256
    ox = case makeIV iv of 
        Just a -> a
        _ -> error "gg" 


