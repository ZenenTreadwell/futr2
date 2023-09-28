module DbT where 
import Test.Hspec
import Prelude as P 
import Nostr.Beam
import Nostr.Filter
import Golden
import Database.SQLite.Simple
import Control.Monad as M

getDbTest = do 
    o <- open "./futr.sqlite" 
    f <- createDb o
    
    insertEv o wev
    return $ describe "database queries" do 
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
  , emptyF {kindsF = Just . Kinds $ [0]}
  ] [
    "Ids1", "Ids2"
    , "Authors"
    , "ETags"
    , "PTags"
    , "Kind 0"
    -- , "Since"
    -- , "Until"
   ]
