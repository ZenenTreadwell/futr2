
module Secp256k1.Internal where

import qualified Data.ByteString.Unsafe as BU
import Foreign                
import Foreign.C              
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)
import System.Entropy 

data XOnlyPubKey64
data Sig64
data PubKey64
data Msg32
data Salt32
data SecKey32
data KeyPair96
type CtxFlags = CUInt
type Ret = CInt
data LCtx
type Ctx = Ptr LCtx

foreign import ccall safe "secp256k1.h secp256k1_context_create" contextCreate
  :: CtxFlags -> IO Ctx

foreign import ccall safe "secp256k1.h secp256k1_keypair_create" keyPairCreate
  :: Ctx -> Ptr KeyPair96 -> Ptr SecKey32 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_keypair_sec" keyPairSec 
  :: Ctx -> Ptr SecKey32 -> Ptr KeyPair96 -> IO Ret
  
foreign import ccall safe "secp256k1.h secp256k1_keypair_pub" keyPairPub 
  :: Ctx -> Ptr PubKey64 -> Ptr KeyPair96 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_ec_pubkey_serialize" ecSerialize 
  :: Ctx -> Ptr p -> Ptr s -> Ptr q -> CUInt -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_ec_pubkey_parse" ecParse 
  :: Ctx -> Ptr pp -> Ptr ppp -> CSize -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_keypair_xonly_pub" keyPairXOnlyPubKey
  :: Ctx -> Ptr PubKey64 -> Ptr Msg32 -> Ptr KeyPair96 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_xonly_pubkey_serialize" schnorrPubKeySerialize
  :: Ctx -> Ptr CUChar -> Ptr XOnlyPubKey64 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_xonly_pubkey_parse" schnorrXOnlyPubKeyParse
  :: Ctx -> Ptr XOnlyPubKey64 -> Ptr CUChar -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_schnorrsig_verify" schnorrSignatureVerify
  :: Ctx -> Ptr Sig64 -> Ptr CUChar -> CSize -> Ptr XOnlyPubKey64 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_schnorrsig_sign32" schnorrSign
  :: Ctx -> Ptr Sig64 -> Ptr Msg32 -> Ptr KeyPair96 -> Ptr Salt32 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_ecdh" ecdh
  ::  Ctx -> Ptr CUChar -> Ptr p -> Ptr s -> 
      Ptr q -> Ptr w -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_ecdsa_verify" ecdsaVerify 
    :: Ctx -> Ptr Sig64 -> Ptr Msg32 -> Ptr PubKey64 -> IO Ret

foreign import ccall safe "secp256k1.h secp256k1_ecdsa_sign" ecdsaSign
    :: Ctx -> Ptr Sig64 -> Ptr Msg32 -> Ptr SecKey32 ->
       Ptr o -> Ptr a -> IO Ret

-- XXX randomize for each sign/verify
ctx :: Ctx
ctx = unsafePerformIO $ contextCreate 0x0301
{-# NOINLINE ctx #-}

getPtr :: ByteString -> IO (Ptr x, CSize)  
getPtr bs = 
    BU.unsafeUseAsCStringLen bs \(p, l) -> pure (castPtr p, fromIntegral l) 

packPtr :: (Ptr x, CSize) -> IO ByteString 
packPtr (p, l) = BU.unsafePackMallocCStringLen (castPtr p, fromIntegral l) 

genSalt :: IO (Ptr x, CSize)
genSalt = getEntropy 32 >>= getPtr 
