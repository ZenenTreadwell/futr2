

import Test.Hspec 
import Nostr
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Crypto.Schnorr(verifyMsgSchnorr, msg, xOnlyPubKey, schnorrSig)

import Data.Text (append, Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Base16 as Hex

main :: IO ()
main = hspec $ do 
  it "creates id" $ shouldBe (eventId ev) evid
  it "create id2" $ shouldBe (eid wev) (eventId . eve $ wev)
  it "loops" $ shouldBe (decode . encode $ ev) (Just ev) 
  it "loops2" $ shouldBe (decode . encode $ wev) (Just wev) 
  it "verifies" $ shouldBe (isValid wev) (True)
  -- it 
  it "key1 " $ flip shouldBe Nothing (xOnlyPubKey . pubkey . eve $ wev)
  it "length1" $ flip shouldBe 32 (BS.length . Hex.decodeLenient . pubkey . eve $ wev)
  it "key2 " $ flip shouldBe Nothing (schnorrSig . sig $ wev)
  it "length2" $ flip shouldBe 64 (BS.length . Hex.decodeLenient . sig $ wev)
  it "key3 " $ flip shouldBe Nothing (msg . eid $ wev)
  it "length3" $ flip shouldBe 32 (BS.length . Hex.decodeLenient . eid $ wev)

  -- it "length4" $ flip shouldBe 32 (BS.length evid)
  -- it "length5" $ flip shouldBe 64 (BS.length esig)



ev = Ev  
    "6e468422dfb74a5738702a8823b9b28168abab8655faacb6853cd0ee15deee93"
    1673347337
    1
    (toJSON [ [String "e", String "3da979448d9ba263864c4d6f14984c423a3838364ec255f03c7904b1ae77f206"]
    , [String "p", String "bf2376e17ba4ec269d10fcc996a4746b451152be9031fa48e74553dde5526bce"]
    ])
    "Walled gardens became prisons, and nostr is the first step towards tearing down the prison walls."         

evid :: ByteString
evid = "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65"

esig :: ByteString
esig = "908a15e46fb4d8675bab026fc230a0e3542bfade63da02d542fb78b2a8513fcd0092619a2c8c1221e581946e0191f2af505dfdf8657a414dbca329186f009262"

wev = Event evid esig ev 
  

hexit b = Hex.decodeLenient b  
