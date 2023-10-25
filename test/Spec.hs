
import Test.Hspec 
import Test.QuickCheck
import JsonLoop
import Golden 
import Nip1
import Nip4
import Nip13
import DbT
          
main :: IO ()
main = do
  runLoops
  nip1 <- nip1GetTest
  nip4 <- getNip4Test
  nip13 <- getNip13Test
  dbT <- getDbTest
  hspec do 
    nip1
    nip4
    nip13
    dbT
