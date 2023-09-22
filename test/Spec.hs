
import Test.Hspec 
import Test.QuickCheck

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

instance Arbitrary Event where 
    arbitrary = Event 
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary

instance Arbitrary Hex32 where 
  arbitrary = Hex32 . BS.pack <$> vectorOf 32 arbitrary 

instance Arbitrary Hex64 where 
  arbitrary = Hex64 . BS.pack <$> vectorOf 64 arbitrary 
  
instance Arbitrary Content where 
  arbitrary = Content <$> arbitrary 
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
  
instance Arbitrary Tag where 
  arbitrary = PTag <$> arbitrary <*> arbitrary 
 
instance Arbitrary Text where 
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Up where 
  arbitrary = oneof 
      [ Submit <$> arbitrary
      , Subscribe <$> arbitrary <*> (pure [])
      , Auth <$> arbitrary 
      , End <$> arbitrary  
      ]

instance Arbitrary Down where 
  arbitrary = oneof
        [ See <$> arbitrary <*> arbitrary
        , Challenge <$> arbitrary
        , Ok <$> arbitrary <*> arbitrary <*> arbitrary
        , Live <$> arbitrary
        , Notice <$> arbitrary
        ]

instance Arbitrary WhyNot where
  arbitrary = oneof 
      [ pure None
      , ServerErr <$> arbitrary
      , Invalid <$> arbitrary
      , Pow <$> arbitrary
      , Duplicate <$> arbitrary
      , Block <$> arbitrary
      , RateLimit <$> arbitrary
      , Restrict <$> arbitrary
      , WhyNot <$> arbitrary
      ] 

instance Arbitrary Filter where 
  arbitrary = Filter <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary Ids where 
  arbitrary = Ids <$> arbitrary
  
instance Arbitrary Authors where 
  arbitrary = Authors <$> arbitrary
  
instance Arbitrary Kinds where 
  arbitrary = Kinds <$> arbitrary 

instance Arbitrary ETagM where 
  arbitrary = ETagM <$> arbitrary 
  
instance Arbitrary PTagM where 
  arbitrary = PTagM <$> arbitrary 
  
instance Arbitrary Since where 
  arbitrary = Since <$> arbitrary 
  
instance Arbitrary Until where 
  arbitrary = Until <$> arbitrary 
  
instance Arbitrary Limit where 
  arbitrary = Limit <$> arbitrary 

loops :: (ToJSON j, FromJSON j, Eq j) => j -> Bool
loops x = case decode . encode $ x of 
    Just x' -> x == x' 
    Nothing -> False

checkx :: Vector (Vector Hex32) -> (Int, Int) -> Bool
checkx kx (i, j) = seek i j == seek j i  
    where 
    seek i' j' = (kx V.! i') V.! j'  

main :: IO ()
main = do
  quickCheck . label "loop event" $ (loops :: Event -> Bool) 
  quickCheck . label "loop filter" $ (loops :: Filter -> Bool) 
  quickCheck $ withMaxSuccess 999 . label "loop down" $ (loops :: Down -> Bool)
  quickCheck . withMaxSuccess 999 . label "loop up" $ (loops :: Up -> Bool)
  kp <- genKeyPair 
  p1 <- exportPub kp
  sec :: Integer <- round <$> getPOSIXTime
  let keyless = Content 1 
                    [ETag evid Nothing (Just Root')] 
                    "garden golgun goodoo"
                    sec
  gems <- signE kp keyless
  let gcont = keyless (pubkey . con $ gems)
  let (kl, d) = mine 11 gcont
  mE <- signE kp kl 
  vEE <- verifyE wev
  mEE <- verifyE mE 
  o <- open "./futr.sqlite"
  createDb o
  insertEv o wev
  kx <- M.replicateM 100 do 
      k' <- genKeyPair 
      p' <- exportPub k'
      pure (k', p')
  let keys = P.map fst kx 
  let pubs = P.map snd kx

  matrikx <-  V.fromList . P.map V.fromList <$> sequenceA [ sequenceA [getShared ki pj 
          | pj <- pubs] 
          | ki <- keys]

  let resultkx = [checkx matrikx (ii', jj') | ii' <- [0..99] , jj' <- [0..99] , ii' > jj' ]
      
      
  k2 <- genKeyPair
  p2 <- exportPub k2
  sh2 <- getShared k2 p1 
  sh1 <- getShared kp p2 
  
  e1 <- encryptE kp p2 "t"

  iv' <- getEntropy 16
  let ccc = createCtx sh1 iv' 
  msg1 <- encryptMsg ccc "test1" 
  let dmsg1 = decryptMsg ccc msg1

  e <- encryptE kp p2 "domino"
  doo <- decryptE k2 e 

  hspec do 
    describe "nip 1" do
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

    describe "database queries" do 
       it "use limit" $ do 
          f' <- P.length <$> fetch o fl
          shouldBe 42 f' 
              
       void $ flip M.mapM ff  \(fi, ti) -> do 
          it ("got some" <> ti) $ do 
              ex <- fetch o fi
              shouldBe (True) ((>0) . P.length $ ex)
          
    describe "nip 13" do 
        it "really counts the leading zeroes" $ shouldBe 36 (difficulty evmine)
        it "REALLY counts the leading zeroes" $ shouldBe 10 (difficulty trickmine)
        it "mine deeply" $ shouldBe ((>= 5) . snd $ mine 5 ev) True
        it "mine deeply" $ shouldBe ((>= 9) . difficulty $ idE . con $ mE) True
        it "show zeros" $ shouldBe (True) (T.isPrefixOf "\"00"  . decodeUtf8 . BS.toStrict . encode $ eid mE)

    describe "nip 4" do 
        it "extract iv" $ shouldBe 16 (BS.length . snd . extract . content . con $ e1)
        it "shared secret ? " $ shouldBe (toJSON sh1) (toJSON sh2) 
        it "always share nicely" $ shouldBe True (L.all id resultkx) 
        it "sometimes share nicely" $ shouldBe True (L.any id resultkx) 
        it "domino" $ shouldBe "domino" doo 
        it "same same" $ shouldBe "test1" dmsg1

        


esig = Hex64 $ Hex.decodeLenient "908a15e46fb4d8675bab026fc230a0e3542bfade63da02d542fb78b2a8513fcd0092619a2c8c1221e581946e0191f2af505dfdf8657a414dbca329186f009262"
wev = Event evid esig ev 
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

evmine = Hex32 $ Hex.decodeLenient "000000000e9d97a1ab09fc381030b346cdd7a142ad57e6df0b46dc9bef6c7e2d"
trickmine = Hex32 $ Hex.decodeLenient "002f79448d9ba263864c4d6f14984c423a3838364ec255f03c7904b1ae77f206"

keyref = Hex32 $ Hex.decodeLenient "bf2376e17ba4ec269d10fcc996a4746b451152be9031fa48e74553dde5526bce"

pub = Hex32 $ Hex.decodeLenient "6e468422dfb74a5738702a8823b9b28168abab8655faacb6853cd0ee15deee93"

evid = Hex32 $ Hex.decodeLenient "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65"

fl = emptyF {kindsF = Just (Kinds [0,1]), limitF = Just (Limit 42)}

ff = 
  [ emptyF {idsF = Just . Ids $ [
    "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65" ]} 
  , emptyF {idsF = Just . Ids $ ["3"]} 
  , emptyF {authorsF = Just . Authors $ ["6"]}
  , emptyF {etagF = Just . ETagM $ [evref]} 
  , emptyF {ptagF = Just . PTagM $ [pub, keyref]}
  -- , emptyF {sinceF = Just . Since $ now } 
  -- , emptyF {untilF = Just . Until . fromIntegral $ now} 
  ] `P.zip`
  [
    "Ids1", "Ids2"
    , "Authors"
    , "ETags"
    , "PTags"
    -- , "Since"
    -- , "Until"
   ]

now = 1693803778