module Ecda where 

import Test.Hspec

import Nostr.Keys
import Secp256k1.Internal
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Golden

getEcdaTest = do 
    
    kp <- genKeyPair
    p1 <- exportPub kp
    (bu, 32) <- getPtr . un32 $ lnpub
    (su, 64) <- getPtr . un64 $ sigTest1 
    (mu, 32) <- getPtr . un32 $ test1
    attest1 <- attest "test2" kp 
    ourattest <- verify32 "test2" attest1 p1
    ourattest64 <- verify "test2" attest1 p1
    verclnsign <- verify32 "test1" sigTest1 lnpub
    verclnsign64 <- verify "test1" sigTest1 lnpub

    n64 <- mallocBytes 64
    (pppp, 96) <- getPtr . un96 $ kp
    reeeee <- ecdsaSign ctx n64 mu pppp nullPtr nullPtr 
    nos64 <- Hex64 <$> packPtr (n64, 64) 
    (pp64, 64) <- parsePub p1 >>= getPtr . un64
    rree64 <- ecdsaVerify ctx n64 mu pp64
    reeecda <- ecdsaVerify ctx su mu bu 

    return $ describe "ecda signatures" do 
        it "verify copleted 32" $ shouldBe 1 reeecda -- True False 
        it "verify completed 64" $ shouldBe 1 rree64
        it "verified ln sign 32" $ shouldBe True verclnsign
        it "verified ln sign 64" $ shouldBe True verclnsign64
        it "verify our attest 32" $ shouldBe True ourattest
        it "verify our attest64" $ shouldBe True ourattest64
        -- it "says signs" $ flip shouldBe 1 reeeee 
