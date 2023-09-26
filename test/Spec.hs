
import Test.Hspec 
import Test.QuickCheck
import JsonLoop
import Golden 
import Nip1
import Nip4
import Nip13
import Ecda

import Prelude as P 
import Data.Text as T
import Data.Vector as V
import Control.Concurrent 
import Data.Time.Clock.POSIX
import Data.Int
import Data.Aeson
import Data.Either
import Data.Function 
import Data.Maybe
import Data.List as L 
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C8
import Secp256k1.Internal
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Base16 as Hex
import Control.Monad as M
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Database.SQLite.Simple
import Nostr.Event
import Nostr.Relay
import Nostr.Beam
import Nostr.Filter
import Nostr.Wire 
import Nostr.Keys
import Nostr.Auth
import Nostr.Direct 
import System.Entropy
import Crypto.Cipher.Types
import Crypto.Hash.SHA256 as SHA256
import Foreign.Marshal.Alloc
import Foreign.Ptr
          
main :: IO ()
main = do
  o <- open "./futr.sqlite" 
  _ <- createDb o
  runLoops
  nip1 <- nip1GetTest
  nip4 <- getNip4Test
  nip13 <- getNip13Test
  ecdaT <- getEcdaTest
  hspec do 
    nip1
    nip4
    nip13
    ecdaT
    describe "database queries" do 
       it "use limit" $ do 
          f' <- P.length <$> fetch o fl
          shouldBe 42 f' 
       void $ flip M.mapM ff  \(fi, ti) -> do 
          it ("got some" <> ti) $ do 
              ex <- fetch o fi
              shouldBe (True) ((>0) . P.length $ ex)
          
    


fl = emptyF {kindsF = Just (Kinds [0,1]), limitF = Just (Limit 42)}

ff = P.zip 
  [ emptyF {idsF = Just . Ids $ [
    "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65" ]} 
  , emptyF {idsF = Just . Ids $ ["3"]} 
  , emptyF {authorsF = Just . Authors $ ["6"]}
  , emptyF {etagF = Just . ETagM $ [evref]} 
  , emptyF {ptagF = Just . PTagM $ [pub, keyref]}
  ] [
    "Ids1", "Ids2"
    , "Authors"
    , "ETags"
    , "PTags"
    -- , "Since"
    -- , "Until"
   ]
