{-|
Module      : Crypto.Schnorr.Internal
License     : MIT
Maintainer  : Sascha-Oliver Prolic <saschaprolic@googlemail.com>
Stability   : experimental
Portability : POSIX

Schnorr signatures from Bitcoin’s secp256k1 library.
-}
module Crypto.Schnorr.Internal where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Unsafe as BU
import           Foreign                (Ptr, castPtr, free, mallocBytes, nullPtr)
import           Foreign.C              (CInt (..), CSize (..), CString, CUChar,
                                         CUInt (..))
import           System.IO.Unsafe       (unsafePerformIO)
import           Control.Monad           (unless)
import qualified Crypto.Hash.SHA256      as SHA256
import           Crypto.Random.DRBG      (CtrDRBG, genBytes, newGen, newGenIO)
import qualified Data.ByteString.Char8   as B8
import           Data.ByteString.UTF8    as BUTF8
import           Data.Either             (fromRight)
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Data.Text               (unpack)
import           System.IO.Unsafe        (unsafePerformIO)

newtype XOnlyPubKey =
  XOnlyPubKey
    { getXOnlyPubKey :: ByteString
    }

newtype SchnorrSig =
  SchnorrSig
    { getSchnorrSig :: ByteString
    }
  deriving (Eq)

newtype PubKey =
  PubKey
    { getPubKey :: ByteString
    }
  deriving (Eq)

newtype KeyPair =
  KeyPair
    { getKeyPair :: ByteString
    }
  deriving (Eq)

newtype Msg =
  Msg
    { getMsg :: ByteString
    }
  deriving (Eq)

newtype SecKey =
  SecKey
    { getSecKey :: ByteString
    }
  deriving (Eq)

-- | Signs a 32-byte 'Msg' using a 'KeyPair'
signMsgSchnorr :: KeyPair -> Msg -> SchnorrSig
signMsgSchnorr (KeyPair sec_key) (Msg m) =
  unsafePerformIO $
  unsafeUseByteString sec_key $ \(sec_key_ptr, 96) ->
    unsafeUseByteString m $ \(msg_ptr, 32) -> do
      sig_ptr <- mallocBytes 64
      ret <- schnorrSign ctx sig_ptr msg_ptr sec_key_ptr nullPtr
      unless (isSuccess ret) $ do
        free sig_ptr
        error "could not schnorr-sign message"
      SchnorrSig <$> unsafePackByteString (sig_ptr, 64)

schnorrPort :: KeyPair -> ByteString -- Maybe XOnlyPubKey
schnorrPort (KeyPair sk) = unsafePerformIO $ 
    unsafeUseByteString sk \(sk', 96) -> do  
        pk' <- mallocBytes 32
        schnorrPubKeySerialize ctx pk' sk'
        unsafePackByteString (pk', 32)
                  
-- | Verifies a schnorr signature.
verifyMsgSchnorr :: XOnlyPubKey -> SchnorrSig -> Msg -> Bool
verifyMsgSchnorr (XOnlyPubKey p) (SchnorrSig s) (Msg m) =
  unsafePerformIO $
  unsafeUseByteString p $ \(pp, _) ->
    unsafeUseByteString s $ \(sp, _) ->
      unsafeUseByteString m $ \(mp, _) ->
        isSuccess <$> schnorrSignatureVerify ctx sp mp 32 pp

-- | Generate a new 'KeyPair'.
generateKeyPair :: IO KeyPair
generateKeyPair = do
  gen <- newGenIO :: IO CtrDRBG
  let Right (randomBytes, newGen) = genBytes 32 gen
  unsafeUseByteString randomBytes $ \(sec_key, _) -> do
    keypair <- mallocBytes 96
    ret <- keyPairCreate ctx keypair sec_key
    if isSuccess ret
      then do
        out <- unsafePackByteString (keypair, 96)
        return $ KeyPair out
      else do
        free keypair
        error "could not generate key pair"




-- | Generate new 'SecKey'.
generateSecretKey :: IO SecKey
generateSecretKey = do
  gen <- newGenIO :: IO CtrDRBG
  let Right (randomBytes, newGen) = genBytes 32 gen
  unsafeUseByteString randomBytes $ \(sec_key_ptr, _) -> do
    ret <- ecSecKeyVerify ctx sec_key_ptr
    if isSuccess ret
      then return (SecKey randomBytes)
      else generateSecretKey

-- | Derive 'SecKey' from 'KeyPair'
deriveSecKey :: KeyPair -> SecKey
deriveSecKey (KeyPair kp) =
  unsafePerformIO $
  unsafeUseByteString kp $ \(kp_ptr, _) -> do
    sec_key_ptr <- mallocBytes 32
    ret <- keyPairSecKey ctx sec_key_ptr kp_ptr
    unless (isSuccess ret) $ do
      free sec_key_ptr
      error "could not compute public key"
    SecKey <$> unsafePackByteString (sec_key_ptr, 32)

-- | Derive 'PubKey' from 'SecKey'.
derivePubKey :: SecKey -> PubKey
derivePubKey (SecKey sec_key) =
  unsafePerformIO $
  unsafeUseByteString sec_key $ \(sec_key_ptr, _) -> do
    pub_key_ptr <- mallocBytes 64
    ret <- ecPubKeyCreate ctx pub_key_ptr sec_key_ptr
    unless (isSuccess ret) $ do
      free pub_key_ptr
      error "could not compute public key"
    PubKey <$> unsafePackByteString (pub_key_ptr, 64)

-- | Derive 'XOnlyPubKey' from 'KeyPair'.
deriveXOnlyPubKey :: KeyPair -> XOnlyPubKey
deriveXOnlyPubKey kp =
  deriveXOnlyPubKeyFromPubKey $ derivePubKey $ deriveSecKey kp

-- | Derive 'XOnlyPubKey' from 'PubKey'.
deriveXOnlyPubKeyFromPubKey :: PubKey -> XOnlyPubKey
deriveXOnlyPubKeyFromPubKey (PubKey bs) =
  unsafePerformIO $
  unsafeUseByteString bs $ \(pub_key_ptr, _) -> do
    x_only_pub_key <- mallocBytes 64
    ret <- xOnlyPubKeyFromPubKey ctx x_only_pub_key nullPtr pub_key_ptr
    if isSuccess ret
      then XOnlyPubKey <$> unsafePackByteString (x_only_pub_key, 64)
      else do
        free x_only_pub_key
        error "could not derive xonly pub key from pub key"

-- schnorrPubKeySerialize

-- decodeHex :: ConvertibleStrings a ByteString => a -> Maybe ByteString
-- decodeHex str =
--   case B16.decodeBase16 $ cs str of
--     Right bs -> Just bs
--     Left _   -> Nothing

-- | Parses a 'XOnlyPubKey' from a given 'ByteString'
xOnlyPubKey :: ByteString -> Maybe XOnlyPubKey
xOnlyPubKey bs
  | BS.length bs == 32 =
    unsafePerformIO $
    unsafeUseByteString bs $ \(input, len) -> do
      pub_key <- mallocBytes 64
      ret <- schnorrXOnlyPubKeyParse ctx pub_key input
      if isSuccess ret
        then do
          out <- unsafePackByteString (pub_key, 64)
          return $ Just $ XOnlyPubKey out
        else do
          return Nothing
  | otherwise = Nothing

-- | Parses a 'KeyPair' from a given 'ByteString'
keypair :: ByteString -> Maybe KeyPair
keypair bs
  | BS.length bs == 96 = Just $ KeyPair {getKeyPair = bs}
  | otherwise = Nothing

-- | Parses a 'SchnorrSig' from a given 'ByteString'
schnorrSig :: ByteString -> Maybe SchnorrSig
schnorrSig bs
  | BS.length bs == 64 = Just $ SchnorrSig {getSchnorrSig = bs}
  | otherwise = Nothing

-- | Import 32-byte 'ByteString' as 'Msg'.
msg :: ByteString -> Maybe Msg
msg bs
  | BS.length bs == 32 = Just $ Msg bs
  | otherwise = Nothing

-- | Import 32-byte 'ByteString' as 'SecKey'.
secKey :: ByteString -> Maybe SecKey
secKey bs
  | BS.length bs == 32 =
    unsafePerformIO $
    unsafeUseByteString bs $ \(ptr, _) -> do
      ret <- ecSecKeyVerify ctx ptr
      if ret == 1
        then return $ Just $ SecKey bs
        else return Nothing
  | otherwise = Nothing

-- | Import 64-byte 'ByteString' as 'PubKey'.
pubKey :: ByteString -> Maybe PubKey
pubKey bs
  | BS.length bs == 64 = Just $ PubKey bs
  | otherwise = Nothing

-- | Combines a 'SecKey' and 'PubKey' into a 'KeyPair'.
combineKeyPair :: SecKey -> PubKey -> KeyPair
combineKeyPair (SecKey s) (PubKey p) = KeyPair (s <> p)

-- | Computes a 'KeyPair' given a 'SecKey'.
keyPairFromSecKey :: SecKey -> KeyPair
keyPairFromSecKey (SecKey s) = KeyPair (s <> p)
  where
    (PubKey p) = derivePubKey (SecKey s)
data XOnlyPubKey64

data SchnorrSig64

data LCtx

data PubKey64

data Msg32

data Sig64

data Compact64

data Seed32

data SecKey32

data KeyPair96

type CtxFlags = CUInt

type SerFlags = CUInt

type Ret = CInt

type NonceFun a
   = Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> Ptr a -> CInt -> IO CInt

type Ctx = Ptr LCtx

verify :: CtxFlags
verify = 0x0101

sign :: CtxFlags
sign = 0x0201

signVerify :: CtxFlags
signVerify = 0x0301

isSuccess :: Ret -> Bool
isSuccess 0 = False
isSuccess 1 = True
isSuccess n = error $ "isSuccess expected 0 or 1 but got " ++ show n

unsafeUseByteString :: ByteString -> ((Ptr a, CSize) -> IO b) -> IO b
unsafeUseByteString bs f =
  BU.unsafeUseAsCStringLen bs $ \(b, l) -> f (castPtr b, fromIntegral l)

useByteString :: ByteString -> ((Ptr a, CSize) -> IO b) -> IO b
useByteString bs f =
  BS.useAsCStringLen bs $ \(b, l) -> f (castPtr b, fromIntegral l)

unsafePackByteString :: (Ptr a, CSize) -> IO ByteString
unsafePackByteString (b, l) =
  BU.unsafePackMallocCStringLen (castPtr b, fromIntegral l)

packByteString :: (Ptr a, CSize) -> IO ByteString
packByteString (b, l) = BS.packCStringLen (castPtr b, fromIntegral l)

ctx :: Ctx
ctx = unsafePerformIO $ contextCreate signVerify
{-# NOINLINE ctx #-}

foreign import ccall safe "secp256k1.h secp256k1_context_create" contextCreate
  :: CtxFlags -> IO Ctx

foreign import ccall safe "secp256k1.h secp256k1_schnorrsig_sign" schnorrSign
  :: Ctx ->
  Ptr SchnorrSig64 -> Ptr Msg32 -> Ptr KeyPair96 -> Ptr a -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_schnorrsig_verify" schnorrSignatureVerify
  :: Ctx ->
  Ptr SchnorrSig64 ->
    Ptr CUChar -> CSize -> Ptr XOnlyPubKey64 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_xonly_pubkey_parse" schnorrXOnlyPubKeyParse
  :: Ctx -> Ptr XOnlyPubKey64 -> Ptr CUChar -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_xonly_pubkey_serialize" schnorrPubKeySerialize
  :: Ctx -> Ptr CUChar -> Ptr XOnlyPubKey64 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_xonly_pubkey_cmp" xOnlyPubKeyCompare
  :: Ctx -> Ptr XOnlyPubKey64 -> Ptr XOnlyPubKey64 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_ec_seckey_verify" ecSecKeyVerify
  :: Ctx -> Ptr SecKey32 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_xonly_pubkey_from_pubkey" xOnlyPubKeyFromPubKey
  :: Ctx -> Ptr XOnlyPubKey64 -> Ptr CInt -> Ptr PubKey64 -> IO Ret

foreign import ccall unsafe "secp256k1.h secp256k1_keypair_create" keyPairCreate
  :: Ctx -> Ptr KeyPair96 -> Ptr SecKey32 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_keypair_sec" keyPairSecKey
  :: Ctx -> Ptr SecKey32 -> Ptr KeyPair96 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_keypair_pub" keyPairPubKey
  :: Ctx -> Ptr PubKey64 -> Ptr KeyPair96 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_keypair_xonly_pub" keyPairXOnlyPubKey
  :: Ctx -> Ptr PubKey64 -> CInt -> Ptr KeyPair96 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_ec_pubkey_create" ecPubKeyCreate
  :: Ctx -> Ptr PubKey64 -> Ptr SecKey32 -> IO Ret