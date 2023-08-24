{-# LANGUAGE
      DeriveAnyClass 
    , StandaloneDeriving
    , TypeFamilies
    , UndecidableInstances 
    , PartialTypeSignatures
    , ImpredicativeTypes
    #-}

module Nostr.Beam where

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Database.Beam.Migrate.Simple
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Data.Text (Text)
import Data.Maybe
import Data.Text.Encoding 
import qualified Data.ByteString as BS 
import Nostr.Event
import Data.Aeson
import Nostr.Db

createDb :: Connection -> IO () 
createDb conn = runBeamSqlite conn $ do 
   veri <- verifySchema migrationBackend spec
   _ <- checkSchema migrationBackend spec mempty
   case veri of 
       VerificationFailed _ -> autoMigrate migrationBackend spec
       VerificationSucceeded -> pure () 

insertEv :: Connection -> Event -> IO ()
insertEv conn e@(Event i _ (Content{..})) = -- do 
    runBeamSqliteDebug print conn $ do
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

    where 
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
insertRelay conn relayText = runBeamSqlite conn $ do
    runInsert $ insert (_relays spec') 
              $ insertValues [Relay relayText]

