module DbT where 
import Test.Hspec
import Prelude as P 
import Nostr.Beam
import Nostr.Filter
import Nostr.Db
import Golden
import Database.SQLite.Simple
import Control.Monad as M
import Control.Concurrent
import Control.Monad.IO.Class
import Nostr.Event
import Nostr.Keys
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time
import Data.Maybe
import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Migrate
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite.Syntax
import Data.Text as T

getDbTest = do 
    kp <- genKeyPair 
    p1 <- exportPub kp
    sec :: Integer <- round <$> getPOSIXTime
    let keyless = Content 11111 
                    [AZTag 't' "butterflies"] 
                    "first"
                    sec

    let keyless2 = Content 11111 
                    [AZTag 't' "butterflies"] 
                    "second"
                    sec
                    
    let keyless3 = Content 33333 
                    [AZTag 'd' "butts"] 
                    "first"
                    sec

    let keyless4 = Content 33333
                    [AZTag 'd' "butts"] 
                    "second"
                    sec
                    
    let keyless5 = Content 22222 
                    [] 
                    "first"
                    sec

    let keyless6 = Content 1 [] "oops" sec

    let keyless7 = Content 0 [] "" sec

    let keyless8 = Content 0 [] "u" sec
    

    mE :: Event <- signE kp keyless 
    mE2 <- signE kp keyless2 
    mE3 <- signE kp keyless3 
    mE4 <- signE kp keyless4 
    mE5 <- signE kp keyless5
    mE6 <- signE kp keyless6
    mE7 <- signE kp keyless7
    mE8 <- signE kp keyless8
    o <- open "./test.sqlite" 
    f <- createDb o
    insertEv o wev
    insertEv o mE
    insertEv o mE2
    insertEv o mE3
    insertEv o mE4
    insertEv o mE5
    insertEv o mE6
    insertEv o mE7
    insertEv o mE8
    
    return $ describe "database queries" do 
       it "replacable" $ do 
          f' <- content . con . P.head 
                <$> fetch o emptyF{kindsF=(Just . Kinds $ [11111])}
          shouldBe "second" f'
       it "replacable 0" $ do 
          f' <- content . con . P.head 
                <$> fetch o emptyF{kindsF=(Just . Kinds $ [0]), authorsF=(Just . Authors $ [wq p1])}
          shouldBe "u" f'
      
       it "parameterized replaceable" $ do 
          f' <- content . con . P.head <$> fetch o emptyF{kindsF=(Just . Kinds $ [33333])}
          shouldBe "second" f'


      
       it "expires shortly?" $ do  
          Just (Just f') <- runBeamSqlite o $ runSelectReturningOne $ select $ do 
              ee <- all_ (_events spec')
              guard_ (_eid ee ==. val_ (wq $ eid mE5)) 
              pure (_expires ee) 
          zo <- getCurrentTimeZone
          ti <- getCurrentTime
          let ow = zonedTimeToLocalTime $ utcToZonedTime zo ti
          let expiresIn = diffLocalTime f' ow
          shouldBe True (fifteen > expiresIn) 
          
       it "use limit" $ do 
          f' <- P.length <$> fetch o fl
          shouldBe 2 f' 
       void $ flip M.mapM ff  \(fi, ti) -> do 
          it ("got some" <> ti) $ do 
              ex <- fetch o fi
              shouldBe (True) ((>0) . P.length $ ex)
          it "got expected count" $ do 
              ex <- fetch o fi
              c : _ <- countFx o fi 
              fromIntegral c `shouldBe` P.length ex

       it "deletes by etag" $ do 
           mmE7 <- signE kp $ Content 5 [ETag (eid mE6) Nothing Nothing] "oooops" sec
           insertEv o mmE7
           aFx <- P.length 
                  <$> fetch o emptyF{ idsF=Just (Ids [T.take 19 . T.drop 1 . wq $ eid mE6]) }
           shouldBe 0 aFx
       it "deletes by atag" $ do 
           mmE8 <- signE kp $ Content 5 [ATag (ARef 33333 p1 (Just "butts")) Nothing] "oooops" sec
           insertEv o mmE8 
           
           aFx <- P.length 
                  <$> fetch o emptyF{aztagF = [AZTag 'd' "butts" ] }
           aFx `shouldBe` 0

        
        
          
fl = emptyF {kindsF = Just (Kinds [0,1]), limitF = Just (Limit 2)}

ff = P.zip 
  [ emptyF {idsF = Just . Ids $ [
    "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65" ]} 
  , emptyF {idsF = Just . Ids $ ["0"]} 
  , emptyF {authorsF = Just . Authors $ ["6"]}
  , emptyF {etagF = Just . ETagM $ [evref]} 
  , emptyF {ptagF = Just . PTagM $ [pub, keyref]}
  , emptyF {kindsF = Just . Kinds $ [0]}
  , emptyF {aztagF = [AZTag 't' "butterflies"]}

  ] [
    "Ids1", "Ids2"
    , "Authors"
    , "ETags"
    , "PTags"
    , "Kind 0"
    , "t tag" 
   ]
