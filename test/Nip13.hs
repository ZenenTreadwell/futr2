module Nip13 where 

import Test.Hspec 
import Golden
import Nostr.Event
import Nostr.Auth 
import Nostr.Keys
import Data.Text as T
import Data.Aeson
import Data.Text.Encoding
import Data.Time.Clock.POSIX
import Data.ByteString as BS

getNip13Test = do 
    kp <- genKeyPair
    p1 <- exportPub kp
    sec :: Integer <- round <$> getPOSIXTime
    mE <- signE kp . fst <$> mine 10 $ Content 1 
                  [ETag evid Nothing (Just Root')] 
                  "garden golgun goodoo"
                  sec
                  p1
    return $ describe "nip 13" do 
        it "really counts the leading zeroes" $ shouldBe 36 (difficulty evmine)
        it "REALLY counts the leading zeroes" $ shouldBe 10 (difficulty trickmine)
        it "mine deeply" $ shouldBe ((>= 5) . snd $ mine 5 ev) True
        it "mine deeply" $ shouldBe ((>= 9) . difficulty $ idE . con $ mE) True
        it "show zeros" $ shouldBe (True) 
            (T.isPrefixOf "\"00"  . decodeUtf8 . BS.toStrict . encode $ eid mE)
