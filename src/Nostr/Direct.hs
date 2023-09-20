module Nostr.Direct where 

import Crypto.Cipher.Types
import Crypto.Cipher.AES (AES256)
-- import Crypto.Error (CryptoFailable(..))
-- import Crypto.PubKey.ECC.Generate (KeyPair(..), KeyPairGenerationParameters(..), generate)
import Data.Bits (xor)
import Data.ByteArray
-- import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Crypto.Error
import System.Entropy 
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Secp256k1.Internal

import Nostr.Keys
-- Generate a random AES initialization vector (IV)
generateIV :: IO ByteString
generateIV = undefined --  getRandomBytes 16


cbc :: ByteArray b => AES256 -> IV AES256 -> b -> b
cbc = cbcEncrypt 

defundCbc :: Hex96 -> Hex32 -> ByteString -> IO Text
defundCbc (Hex96 q) (Hex32 w) msg = do
    -- pub <- parsePub q 
    let len = BS.length msg
        rem = mod len 16 
        msg' = msg <> BS.replicate (16 - rem) 0

    print "quickchceck"
    print $ len 
    print $ BS.length msg'     
    
    sh <- mallocBytes 32 

    (sec', 32) <- getPtr . BS.take 32 $ q 

    (pub', 32) <- getPtr w 

    ret <- ecdh ctx sh pub' sec' nullPtr nullPtr  
    
    print ret 
    
    shBs <- packPtr (sh, 32)
    
    let xo :: AES256
        xo = case cipherInit shBs :: CryptoFailable AES256 of 
            CryptoPassed a -> a 
            _ -> error "ff"
            
    iv <- getEntropy 16
    let ox :: IV AES256
        ox = case makeIV iv of 
            Just a -> a
            _ -> error "gg" 
    
    let t = encodeBase64' $ cbc xo ox msg'
    
    let fin = (decodeUtf8 t)
               <> "iv?=" <> 
              (decodeUtf8 . encodeBase64' $ iv)
    print fin 
    pure fin 

    

-- try2 :: _ 
-- try2 =  


  


-- en = B64.



-- cbc :: BlockCipher AES256 -> _ 
-- cbc x = cbcEncrypt x 

-- -- Direct encryption function
-- direct :: Hex96 -> Hex32 -> Text -> Text
-- direct (Hex96 senderPrivateKey) (Hex32 recipientPublicKey) plaintext = do
  -- Convert private and public keys to the necessary formats
  -- let senderKeyPair = generateKeyPairFromPrivateKey senderPrivateKey
  -- --     recipientPublicKeyBytes = B64.decodeLenient (T.encodeUtf8 recipientPublicKey)
  
  -- -- Generate a random IV
  -- iv <- generateIV

  -- -- Your plaintext message
  -- let plaintextBytes = encodeUtf8 plaintext

  -- -- Encrypt the message
  -- let encryptedMessage = encryptAES256CBC senderPrivateKey iv plaintextBytes
  -- let ivBase64 = B64.encode iv

  -- pure encryptedMessage

  -- Combine the encrypted message and IV as a Text
  -- let encryptedText = T.pack (C8.unpack encryptedMessage <> "?iv=" <> C8.unpack ivBase64)
  
  -- return encryptedText

-- -- Helper function to generate a key pair from a private key
-- generateKeyPairFromPrivateKey :: ByteString -> KeyPair
-- generateKeyPairFromPrivateKey privateKeyBytes =
--   let curve = getCurveByName SEC_p256k1
--       params = KeyPairGenerationParameters curve privateKeyBytes
--   in generate params

-- -- import Crypto.Cipher.AES (AES256)
-- -- import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), CipherCtx(..), nullIV)
-- -- import Crypto.Error (CryptoFailable(..))
-- -- import Crypto.PubKey.ECC.ECDSA (PrivateKey(..), PublicKey(..), Signature(..))
-- -- import Crypto.PubKey.ECC.Generate (generate, KeyPairGenerationParameters(..))
-- -- import Crypto.PubKey.ECC.Prim (getCurveByName)
-- -- import Crypto.Random (getRandomBytes)
-- -- import Data.Bits (xor)
-- -- import Data.ByteArray (convert)
-- -- import Data.ByteString (ByteString)
-- -- import qualified Data.ByteString as BS
-- -- import qualified Data.ByteString.Base64 as B64
-- -- import qualified Data.ByteString.Char8 as C8
-- -- import Data.Monoid ((<>))
-- -- import Data.Text (Text)
-- -- import qualified Data.Text as T

-- -- -- Generate a random AES initialization vector (IV)
-- generateIV :: IO ByteString
-- generateIV = getRandomBytes 16

-- -- Encrypt a message using AES-256-CBC
-- encryptAES256CBC :: ByteString -> ByteString -> ByteString -> ByteString
-- encryptAES256CBC key iv plaintext =
--   let cipher :: AES256
--       cipher = undefined  -- You'll need to initialize the AES cipher here
      
--       padPlaintext :: ByteString
--       padPlaintext = pkcs7Pad 16 plaintext
      
--       cipherCtx :: CipherCtx AES256
--       cipherCtx = cipherInit key
      
--       ciphertext :: ByteString
--       ciphertext = cbcEncrypt cipherCtx iv padPlaintext
      
--       ivBase64 :: ByteString
--       ivBase64 = B64.encode iv
--   in ciphertext <> "?iv=" <> ivBase64

-- -- PKCS7 padding
-- pkcs7Pad :: Int -> ByteString -> ByteString
-- pkcs7Pad blockSize bs =
--   let paddingLen = blockSize - (BS.length bs `mod` blockSize)
--       paddingByte = fromIntegral paddingLen
--   in bs <> BS.replicate paddingLen paddingByte

-- -- Direct encryption function
-- direct :: Hex96 -> Hex32 -> Text -> Text
-- direct (Hex96 senderPrivateKey) (Hex32 recipientPublicKey) plaintext = do
--   -- Convert private and public keys to the necessary formats
--   let senderKeyPair = generateKeyPairFromPrivateKey senderPrivateKey
--       recipientPublicKeyBytes = B64.decodeLenient (T.encodeUtf8 recipientPublicKey)

--   -- Generate a random IV
--   iv <- generateIV

--   -- Your plaintext message
--   let plaintextBytes = T.encodeUtf8 plaintext

--   -- Encrypt the message
--   let encryptedMessage = encryptAES256CBC senderPrivateKey iv plaintextBytes
--   let ivBase64 = B64.encode iv

--   -- Combine the encrypted message and IV as a Text
--   let encryptedText = T.pack (C8.unpack encryptedMessage <> "?iv=" <> C8.unpack ivBase64)
  
--   return encryptedText

-- -- Helper function to generate a key pair from a private key
-- generateKeyPairFromPrivateKey :: ByteString -> (PublicKey, PrivateKey)
-- generateKeyPairFromPrivateKey privateKeyBytes =
--   let curve = getCurveByName SEC_p256k1
--       private = PrivateKey curve privateKeyBytes
--       public = publicKey private
--   in (public, private)

