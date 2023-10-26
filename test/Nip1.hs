module Nip1 where 

import Test.Hspec
import Golden 
import Nostr.Event
import Nostr.Keys
import Nostr.Filter
import Data.ByteString as BS
import Data.Time.Clock.POSIX
import Nostr.Beam

nip1GetTest = do
    kp <- genKeyPair 
    p1 <- exportPub kp
    sec :: Integer <- round <$> getPOSIXTime
    let keyless = Content 1 
                    [ETag evid Nothing (Just Root')] 
                    "garden golgun goodoo"
                    sec
    mE <- signE kp keyless 
    vEE <- verifyE wev
    mEE <- verifyE mE 
    return $ describe "nip 1" do
      it "gold id" $ flip shouldBe evid (idE ev)
      it "verifiesE!" $ shouldBe vEE True
      it "signs valid 2" $ flip shouldBe True mEE 
      it "evid length" $ flip shouldBe 32 (BS.length . un32 $ evid)
      it "length1" $ flip shouldBe 32 (BS.length . un32 . pubkey . con $ wev)
      it "length2" $ flip shouldBe 64 (BS.length . un64 . sig $ wev)
      it "length3" $ flip shouldBe 32 (BS.length . un32 . eid $ wev)
      it "msign sig length" $ shouldBe 64 (BS.length . un64 . sig $ mE )
      it "msign eid length" $ shouldBe 32 (BS.length . un32 . eid $ mE )
      it "msign pub length" $ shouldBe 32 (BS.length . un32 . pubkey . con $ mE)
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
      it "matches f" $ shouldBe True (matchF wev (emptyF{idsF = Just $ Ids ["437"]}))
      it "no matches f" $ shouldBe False (matchF wev (emptyF{idsF = Just $ Ids ["37"]}))
      it "npubs work" $ shouldBe banswer (npub bexample)
      it "npubs work 2" $ shouldBe bexample (xnpub banswer) 
