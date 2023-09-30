module Ecda where 

import Test.Hspec

import Nostr.Keys
import Secp256k1.Internal
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C
import Foreign
import Golden
import Data.ByteString.Base16 as Hex
import Data.ByteString as BS

getEcdaTest = do 
    
    kp <- genKeyPair
    p1 <- exportPub kp
    -- p2 <- parsePub p1
    -- (p2', 64) <- getPtr . un64 $ p2 
    (salt, 32) <- genSalt
    keypair <- mallocBytes 96
    keyPairCreate ctx keypair salt
    salt' <- mallocBytes 32
    keyPairSec ctx salt' keypair 

    keysalt <- packPtr (salt', 32)
    keysalt2 <- packPtr (salt, 32)
    
    kp2 <- genKeyPair
    p2 <- exportPub kp2
    (p2', 33) <-  getPtr . (Hex.decodeLenient "02" <>) . un32 $ p2

    
    
    (kp', 96) <- getPtr . un96 $ kp
    (p1', 33) <- getPtr . (Hex.decodeLenient "02" <>) . un32 $ p1 

    -- XXX this explodes malloc(): corrupted top size 
    -- but 32 / 96 seems certainly correct
    se <- mallocBytes 32 
    keyPairSec ctx se kp' 

    -- (se, 32) <- getPtr . BS.take 32 . un96 $ kp

    nba <- mallocBytes 65 
    nbe <- mallocBytes 65 ---XXX big enough ?
    nbc <- mallocBytes 33 
    nbd <- mallocBytes 32 
    let value :: CSize
        value = 33 
    ptr <- mallocBytes 32 
    poke ptr value
    -- keyPairPub ctx nbe kp'
    -- parse1 <- ecParse ctx nbe p1' 32
    parse2 <- ecParse ctx nbe p1' 33
    ecs <- ecSerialize ctx nbc ptr nbe 258  

    compres <- packPtr (nbc, 33)
    compres' <- packPtr (p1', 33)

    sh <- mallocBytes 32 
    r <- ecdh ctx sh nbc se nullFunPtr nullPtr  
    
    return $ describe "ec" do 
        -- it "ones" $ shouldBe 1 parse1
        it "ones ecParse" $ shouldBe 1 parse2
        it "ones ecdh" $ shouldBe 1 r
        it "ones ecs" $ shouldBe 1 ecs
        it "salt is key" $ shouldBe keysalt keysalt2
        it "compressed is this" $ shouldBe compres compres'

    

    
    -- (bu, 32) <- getPtr . un32 $ lnpub
    -- (su, 64) <- getPtr . un64 $ sigTest1 
    -- (mu, 32) <- getPtr . un32 $ test1
    -- attest1 <- attest "test2" kp 
    -- ourattest <- verify32 "test2" attest1 p1
    -- ourattest64 <- verify "test2" attest1 p1
    -- verclnsign <- verify32 "test1" sigTest1 lnpub
    -- verclnsign64 <- verify "test1" sigTest1 lnpub

    -- n64 <- mallocBytes 64
    -- (pppp, 96) <- getPtr . un96 $ kp
    -- reeeee <- ecdsaSign ctx n64 mu pppp nullPtr nullPtr 
    -- nos64 <- Hex64 <$> packPtr (n64, 64) 
    -- (pp64, 64) <- parsePub p1 >>= getPtr . un64
    -- rree64 <- ecdsaVerify ctx n64 mu pp64
    -- reeecda <- ecdsaVerify ctx su mu bu 

    -- return $ describe "ecda signatures" do 
    --     it "verify copleted 32" $ shouldBe 1 reeecda -- True False 
    --     it "verify completed 64" $ shouldBe 1 rree64
    --     it "verified ln sign 32" $ shouldBe True verclnsign
    --     it "verified ln sign 64" $ shouldBe True verclnsign64
    --     it "verify our attest 32" $ shouldBe True ourattest
    --     it "verify our attest64" $ shouldBe True ourattest64
    --     -- it "says signs" $ flip shouldBe 1 reeeee 
