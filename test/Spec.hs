
import Test.Hspec 

import Nostr.Event
import Nostr.Relay
import Nostr.Beam
import Nostr.Filter
import Nostr.Wire 
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C8
import Secp256k1.Internal
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Base16 as Hex
import Control.Monad.IO.Class

main :: IO ()
main = do
  kp <- genKeyPair 
  sec :: Integer <- round <$> getPOSIXTime
  let keyless = Content 1 
                    [ETag evid Nothing (Just Root')] 
                    "garden golgun goodoo"
                    sec
  mE <- signE kp keyless 
  vEE <- verifyE wev
  mEE <- verifyE mE 
  hspec do 
    describe "correctly hashes event" do
      it "gold id" $ flip shouldBe evid (idE ev)

    describe "json encoding and decoding" do
      it "loops ev" $ shouldBe (decode . encode $ ev) (Just ev) 
      it "loops event" $ shouldBe (decode . encode $ wev) (Just wev) 
      it "loops up" $ flip shouldBe wev $
          case decode . encode $ Submit wev of 
            Just (Submit e) -> e 
            _ -> undefined
      it "loops down" $ flip shouldBe wev $ 
          case decode . encode $ See "1" wev of 
            Just (See _ e) -> e
            _ -> undefined 
      it "loops filter" $ flip shouldBe ff $
          case decode . encode $ ff of 
            Just fff -> fff
            _ -> undefined 

    describe "validates with schnorr" do
      it "verifiesE!" $ shouldBe vEE True
      -- it "isValid!" $ shouldBe (isValid wev) True

    describe "signs with schnorr" do
      -- it "signs valid" $ flip shouldBe True (isValid mE) 
      it "signs valid 2" $ flip shouldBe True mEE 

    describe "what the length is it anyway" do
      it "evid length" $ flip shouldBe 32 (BS.length . un32 $ evid)
      it "length1" $ flip shouldBe 32 (BS.length . un32 . pubkey . con $ wev)
      it "length2" $ flip shouldBe 64 (BS.length . un64 . sig $ wev)
      it "length3" $ flip shouldBe 32 (BS.length . un32 . eid $ wev)
      it "msign sig length" $ shouldBe 64 (BS.length . un64 . sig $ mE )
      it "msign eid length" $ shouldBe 32 (BS.length . un32 . eid $ mE )
      it "msign pub length" $ shouldBe 32 (BS.length . un32 . pubkey . con $ mE)
      --it "show pubkey (that works)" $ flip shouldBe Nothing (xOnlyPubKey . un32 . pubkey . con $ wev)
    describe "matchM checks" do 
      it "matches by id" $ shouldBe True (matchM wev (Ids ["37", "437"]))
      it "doesn't match by id" $ shouldBe False (matchM wev (Ids ["37"]))
      it "matches by author" $ shouldBe True (matchM wev (Authors ["6", "7"])) 
      it "matches by author" $ shouldBe False (matchM wev (Authors ["7"])) 
      it "matches by etag" $ shouldBe True (matchM wev (ETagM [evref]))
      it "matches by ptag" $ shouldBe True (matchM wev (PTagM [keyref]))
      it "matches since" $ shouldBe True (matchM wev (Since 1673347336))
      it "matches since" $ shouldBe False (matchM wev (Since 1673347338))
      it "matches until" $ shouldBe False (matchM wev (Until 1673347336))
      it "matches until" $ shouldBe True (matchM wev (Until 1673347338))

    describe "matchF check" do 
      it "matches f" $ shouldBe True (matchF wev (emptyF{idsF = Just $ Ids ["437"]}))
      it "no matches f" $ shouldBe False (matchF wev (emptyF{idsF = Just $ Ids ["37"]}))



ev = Content
    1
    [ 
      ETag evref Nothing Nothing 
    , PTag keyref Nothing  
    ] 
    "Walled gardens became prisons, and nostr is the first step towards tearing down the prison walls."         
    1673347337
    pub

evref = Hex32 $ Hex.decodeLenient "3da979448d9ba263864c4d6f14984c423a3838364ec255f03c7904b1ae77f206"

keyref = Hex32 $ Hex.decodeLenient "bf2376e17ba4ec269d10fcc996a4746b451152be9031fa48e74553dde5526bce"

pub = Hex32 $ Hex.decodeLenient "6e468422dfb74a5738702a8823b9b28168abab8655faacb6853cd0ee15deee93"

evid = Hex32 $ Hex.decodeLenient "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65"

esig = Hex64 $ Hex.decodeLenient "908a15e46fb4d8675bab026fc230a0e3542bfade63da02d542fb78b2a8513fcd0092619a2c8c1221e581946e0191f2af505dfdf8657a414dbca329186f009262"

wev = Event evid esig ev 

ff = emptyF {kindsF = Just (Kinds [0,1]), limitF = Just (Limit 42)}


