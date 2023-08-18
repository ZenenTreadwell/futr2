
module Secp256k1.Internal where

import qualified Data.ByteString.Unsafe as BU
import           Foreign                
import           Foreign.C              
import           System.IO.Unsafe       (unsafePerformIO)
import           Data.ByteString.UTF8    as BUTF8

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

type Ctx = Ptr LCtx

foreign import ccall safe "secp256k1.h secp256k1_context_create" contextCreate
  :: CtxFlags -> IO Ctx

foreign import ccall safe "secp256k1.h secp256k1_schnorrsig_sign32" schnorrSign
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

-- foreign import ccall safe "secp256k1.h secp256k1_xonly_pubkey_cmp" xOnlyPubKeyCompare
--   :: Ctx -> Ptr XOnlyPubKey64 -> Ptr XOnlyPubKey64 -> IO Ret
-- foreign import ccall safe "secp256k1.h secp256k1_ec_seckey_verify" ecSecKeyVerify
--   :: Ctx -> Ptr SecKey32 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_xonly_pubkey_from_pubkey" xOnlyPubKeyFromPubKey
  :: Ctx -> Ptr XOnlyPubKey64 -> Ptr CInt -> Ptr PubKey64 -> IO Ret

foreign import ccall unsafe "secp256k1.h secp256k1_keypair_create" keyPairCreate
  :: Ctx -> Ptr KeyPair96 -> Ptr SecKey32 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_keypair_sec" keyPairSecKey
  :: Ctx -> Ptr SecKey32 -> Ptr KeyPair96 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_keypair_pub" keyPairPubKey
  :: Ctx -> Ptr PubKey64 -> Ptr KeyPair96 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_keypair_xonly_pub" keyPairXOnlyPubKey
  :: Ctx -> Ptr PubKey64 -> Ptr CInt -> Ptr KeyPair96 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_ec_pubkey_create" ecPubKeyCreate
  :: Ctx -> Ptr PubKey64 -> Ptr SecKey32 -> IO Ret

verify :: CtxFlags
verify = 0x0101

sign :: CtxFlags
sign = 0x0201

signVerify :: CtxFlags
signVerify = 0x0301

ctx :: Ctx
ctx = unsafePerformIO $ contextCreate signVerify
{-# NOINLINE ctx #-}

getPtr :: ByteString -> IO (Ptr x, CSize)  
getPtr bs = 
    BU.unsafeUseAsCStringLen bs \(p, l) -> pure (castPtr p, fromIntegral l) 

packPtr :: (Ptr x, CSize) -> IO ByteString 
packPtr (p, l) = BU.unsafePackMallocCStringLen (castPtr p, fromIntegral l) 
