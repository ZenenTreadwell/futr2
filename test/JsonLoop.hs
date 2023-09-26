module JsonLoop where

import Test.QuickCheck
import Data.ByteString as BS
import Data.Text as T
import Data.Aeson
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Wire

runLoops :: IO () 
runLoops = do 
    quickCheck . label "loop event" $ (loops :: Event -> Bool) 
    quickCheck . label "loop filter" $ (loops :: Filter -> Bool) 
    quickCheck $ withMaxSuccess 999 . label "loop down" $ (loops :: Down -> Bool)
    quickCheck . withMaxSuccess 999 . label "loop up" $ (loops :: Up -> Bool)


loops :: (ToJSON j, FromJSON j, Eq j) => j -> Bool
loops x = case decode . encode $ x of 
    Just x' -> x == x' 
    Nothing -> False

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
