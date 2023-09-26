module Nip4 where 

import Prelude as P
import Test.Hspec
import Nostr.Keys
import Nostr.Event
import Nostr.Auth
import Nostr.Direct
import Data.Vector as V
import Control.Monad as M
import Data.List as L
import Data.ByteString as BS
import System.Entropy

type Kx = Vector (Vector Hex32)

checkx :: Kx -> (Int, Int) -> Bool
checkx kx (i, j) = seek i j == seek j i  
    where 
    seek i' j' = (kx V.! i') V.! j'  

boolKx :: Kx -> [[Bool]]
boolKx kx = [[checkx kx (i', j') 
                | i' <- [0..9] ]
                | j' <- [0..9] ]

printKx :: Kx -> IO [()] 
printKx = M.mapM (print . P.map boolToChar) . boolKx  

boolToChar :: Bool -> Char
boolToChar True = 'X'
boolToChar False = 'O'

getNip4Test = do 
    k1 <- genKeyPair 
    p1 <- exportPub k1
    k2 <- genKeyPair
    p2 <- exportPub k2
    e1 <- encryptE k1 p2 "t"
    sh2 <- getShared k2 p1 
    sh1 <- getShared k1 p2 
    kx <- M.replicateM 100 do 
        k' <- genKeyPair 
        p' <- exportPub k'
        pure (k', p')
    let keys = P.map fst kx 
    let pubs = P.map snd kx
    matrikx <- V.fromList . P.map V.fromList <$> sequenceA [ sequenceA  [ getShared ki pj 
            | pj <- pubs] 
            | ki <- keys]
    let resultkx = [ checkx matrikx (ii', jj') 
            | ii' <- [0..99] , jj' <- [0..99] , ii' > jj' ]

    ccc <- createCtx sh1 <$> getEntropy 16

    doo <- encryptE k1 p2 "domino" >>= decryptE k2
    dmsg1 <- decryptMsg ccc <$> encryptMsg ccc "domina"

    printKx matrikx
    
    return $ describe "nip 4" do 
        it "extract iv" $ shouldBe 16 (BS.length . snd . extract . content . con $ e1)
        it "shared secret ? " $ shouldBe sh1 sh2 
        it "always share nicely" $ shouldBe True (L.all id resultkx) 
        it "sometimes share nicely" $ shouldBe True (L.any id resultkx) 
        it "domino" $ shouldBe "domino" doo 
        it "domina" $ shouldBe "domina" dmsg1
