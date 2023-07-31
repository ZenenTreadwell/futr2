
import Test.Hspec 
import Nostr
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C8
import Crypto.Schnorr --(verifyMsgSchnorr, msg, xOnlyPubKey, schnorrSig)

import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified "base16-bytestring" Data.ByteString.Base16 as Hex
import qualified "base16" Data.ByteString.Base16 as Hex2

main :: IO ()
main = do
  kp <- generateKeyPair 
  -- putStrLn . show $ kp
  
  let bkey :: ByteString
      bkey = getXOnlyPubKey . deriveXOnlyPubKey $ kp 
      msign :: Maybe Event    
      msign = signEv kp ev{pubkey=bkey}

      bkey2 = xOnlyPubKey bkey
      
  hspec $ do 
    it "evid length" $ flip shouldBe 32 (BS.length evid)
    
    it "gold id" $ flip shouldBe evid (eventId ev)

    it "gold id 2" $ shouldBe (eid wev) (eventId . eve $ wev)
    it "loops" $ shouldBe (decode . encode $ ev) (Just ev) 
    it "loops 2" $ shouldBe (decode . encode $ wev) (Just wev) 
    
    it "show pubkey (that works)" $ flip shouldBe Nothing (xOnlyPubKey . pubkey . eve $ wev)
    it "verifies!" $ shouldBe (isValid wev) (Just True)

    it "bkey length" $ flip shouldBe 32 (BS.length bkey)
    it "bkey2 " $ flip shouldNotBe Nothing bkey2
    it "msign sig length" $ shouldBe 64 (maybe 0 id $ BS.length . sig <$> msign )
    it "msign eid length" $ shouldBe 32 (maybe 0 id $ BS.length . eid <$> msign )
    
    it "signs valid" $ flip shouldBe (Just True) (isValid =<< msign) 

    it "sign (verbose)" $ flip shouldBe (Nothing) (msign) 
      
    it "signs, but wrong" $ flip shouldBe (Just False) $ isValid =<< (signEv kp ev) 

    it "length1" $ flip shouldBe 32 (BS.length . pubkey . eve $ wev)
    it "length2" $ flip shouldBe 64 (BS.length . sig $ wev)
    it "length3" $ flip shouldBe 32 (BS.length .eid $ wev)

    -- 
    -- it "key1 " $ flip shouldBe Nothing (xOnlyPubKey . pubkey . eve $ wev)
    -- it "key2 " $ flip shouldBe Nothing (schnorrSig . sig $ wev)
    -- it "key3 " $ flip shouldBe Nothing (msg . eid $ wev)

ev = Ev  
    pub
    1673347337
    1
    (toJSON [ [String "e", String "3da979448d9ba263864c4d6f14984c423a3838364ec255f03c7904b1ae77f206"]
    , [String "p", String "bf2376e17ba4ec269d10fcc996a4746b451152be9031fa48e74553dde5526bce"]
    ])
    "Walled gardens became prisons, and nostr is the first step towards tearing down the prison walls."         

pub :: ByteString 
pub = Hex.decodeLenient "6e468422dfb74a5738702a8823b9b28168abab8655faacb6853cd0ee15deee93"

evid :: ByteString
evid = Hex.decodeLenient "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65"

esig :: ByteString
esig = Hex.decodeLenient "908a15e46fb4d8675bab026fc230a0e3542bfade63da02d542fb78b2a8513fcd0092619a2c8c1221e581946e0191f2af505dfdf8657a414dbca329186f009262"

wev = Event evid esig ev 
