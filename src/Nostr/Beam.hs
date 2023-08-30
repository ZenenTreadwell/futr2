{-# LANGUAGE
      DeriveAnyClass 
    , StandaloneDeriving
    , TypeFamilies
    , UndecidableInstances 
    , PartialTypeSignatures
    , ImpredicativeTypes
    , DuplicateRecordFields
    #-}

module Nostr.Beam where

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple as SQL
import Database.Beam.Migrate.Simple
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Data.Text (Text)
import Data.Maybe
import Data.Text.Encoding 
import qualified Data.ByteString as BS 
import Nostr.Event
import Nostr.Filter
import Data.Aeson
import Nostr.Db
import Control.Monad.State
import Control.Exception

createDb :: SQL.Connection -> IO () 
createDb conn = runBeamSqlite conn $ do 
   veri <- verifySchema migrationBackend spec
   _ <- checkSchema migrationBackend spec mempty
   case veri of 
       VerificationFailed _ -> autoMigrate migrationBackend spec
       VerificationSucceeded -> pure () 


insertPl :: Connection -> Event -> IO () 
insertPl conn e@(Event i _ (Content{..})) = 
    runBeamSqlite conn 
        $ runUpdate
        $ save (_plebs spec') (Pleb (wq pubkey) (Just $ wq e) ) 

insertEv :: Connection -> Event -> IO ()
insertEv conn e@(Event i _ (Content{..})) = catch runIns \(e :: SQLError)-> do 
        print "SQLERROR!!!!"
        print e 
    where
    runIns = runBeamSqlite conn $ do
    
        runInsert $ insertOnConflict (_plebs spec') 
                                     (insertExpressions [Pleb (val_ $ wq pubkey) default_])
                                      anyConflict
                                      onConflictDoNothing
       
                                                                                               
        runInsert $ insert (_events spec') (insertValues [toEv e])
        let (mx, rx) = gather . catMaybes $ flip map tags \case
                ETag ie _ marker -> Just . Right $ (ie, marker)
                PTag ip _ -> Just . Left $ ip
                _ -> Nothing 

        
        runInsert . insert (_mentions spec') . insertExpressions $ map mention mx
        runInsert . insert (_replies spec') . insertExpressions $ map (uncurry reply) rx

    gather :: [Either a b] ->  ([a], [b]) 
    gather = foldr g ([],[]) 
        where 
        g (Right r) (mx, rx) = (mx, r:rx)  
        g (Left l) (mx, rx) = (l:mx, rx)
    
    reply :: Hex32 -> Maybe Marker -> ReplyT (QExpr Sqlite m) 
    reply id' marker = 
        Reply default_ (val_ . EvId . wq $ i) (val_ . wq $ id') (val_ marker)
    
    mention :: Hex32 -> MentionT (QExpr Sqlite m) 
    mention id' = Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id') 

toEv :: Event -> EvT Identity 
toEv e = Ev 
      (wq $ eid e) 
      (PlebId . wq . pubkey . con $ e) 
      (fromInteger . created_at . con $ e) 
      (wq e)

wq :: ToJSON a => a -> Text 
wq = decodeUtf8 . BS.toStrict . encode 

insertId :: Connection -> Text -> IO ()
insertId conn privKey = runBeamSqlite conn $
    runInsert $ insert (_identities spec') 
              $ insertValues [Id privKey]

insertRelay :: Connection -> Text -> IO ()
insertRelay db uri = runBeamSqlite db $ do
    runInsert $ insert (_relays spec') 
              $ insertExpressions 
              [ Relay default_  (val_ uri) (val_ False) ]



              
               


-- fetch :: Connection -> Filter -> _
-- fetch c (Filter mx l) = runSelectReturningList . selectWith $ do 
--     all_ 
    



  
    






