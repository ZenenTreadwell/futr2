{-# LANGUAGE
      DeriveAnyClass 
    , StandaloneDeriving
    , TypeFamilies
    , UndecidableInstances 
    , PartialTypeSignatures
    , ImpredicativeTypes
    #-}
    -- DeriveGeneric 
    -- , FlexibleContexts
    -- , FlexibleInstances 
    -- , TypeSynonymInstances 
    -- , UndecidableInstances
    -- , MultiParamTypeClasses

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
import GHC.Utils.Misc (uncurry3)
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
        -- XXX n as a Transaction??

        runInsert $ insertOnConflict (_plebs spec') 
                                     (insertExpressions [Pleb (val_ $ wq pubkey) default_])
                                      anyConflict
                                      onConflictDoNothing
        
        runInsert $ insert (_events spec') (insertValues [toEv e])
        let (mx, rx) = gather . catMaybes $ flip map tags \case
                ETag id _ marker -> Just . Right $ (i, id, marker)
                PTag id _ -> Just . Left $ (i, id)
                _ -> Nothing 

        runInsert . insert (_mentions spec') . insertExpressions $ map (uncurry mention) mx
        runInsert . insert (_replies spec') . insertExpressions $ map (uncurry3 reply) rx

    where 
    gather :: [Either a b] ->  ([a], [b]) 
    gather = foldr g ([],[]) 
        where 
        g (Right r) (mx, rx) = (mx, r:rx)  
        g (Left l) (mx, rx) = (l:mx, rx)
    
    reply i id marker = 
        Reply default_ (val_ . EvId . wq $ i) (val_ . wq $ id) (val_ marker)
    
    mention :: Hex32 -> Hex32 -> MentionT (QExpr Sqlite m) 
    mention i id = Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id) 

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

