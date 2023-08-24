{-# LANGUAGE
      DeriveAnyClass 
    , StandaloneDeriving
    , TypeFamilies
    , UndecidableInstances 
    , PartialTypeSignatures
    #-}
    -- DeriveGeneric 
    -- , FlexibleContexts
    -- , FlexibleInstances 
    -- , TypeSynonymInstances 
    -- , ImpredicativeTypes 
    -- , UndecidableInstances
    -- , MultiParamTypeClasses
module Nostr.Db where

-- -- import Data.Aeson
-- -- import Data.Int
-- -- import Data.Text
-- import Database.Beam
-- import Database.Beam.Sqlite
-- import Database.SQLite.Simple
-- import Database.Beam.Migrate
-- import Database.Beam.Migrate.Simple
-- import Database.Beam.Backend.SQL.BeamExtensions
-- import Database.Beam.Backend.SQL
-- import Database.Beam.Sqlite.Syntax
-- import Database.Beam.Sqlite.Migrate (migrationBackend)
-- import Data.Int
-- import Data.Text (Text)
-- import Data.Maybe
-- import qualified Data.Text as T 
-- import Data.Text.Encoding 
-- import qualified Data.ByteString as BS 
-- import Control.Monad.State
-- import Nostr.Event
-- import Data.Aeson
-- import Control.Exception as E 

-- spec :: CheckedDatabaseSettings Sqlite Db
-- spec = defaultMigratableDbSettings 
-- spec' :: DatabaseSettings Sqlite Db
-- spec' = defaultDbSettings

-- createDb :: Connection -> IO () 
-- createDb conn = runBeamSqlite conn $ do 
--    veri <- verifySchema migrationBackend spec
--    _ <- checkSchema migrationBackend spec mempty
--    case veri of 
--        VerificationFailed _ -> autoMigrate migrationBackend spec
--        VerificationSucceeded -> pure () 

-- type T = TableEntity
-- data Db f = Db {
--         _events :: f (T EvT)
--       , _identities :: f (T IdT)
--       , _relays :: f (T RelayT)
--       , _plebs :: f (T PlebT)
--       , _replies :: f (T ReplyT)
--       , _mentions :: f (T MentionT)
--       } deriving (Generic, Database Sqlite)

-- data EvT f = Ev {
--         _eid :: C f Text 
--       , _pub :: PrimaryKey PlebT f
--       , _time :: C f Int64
--       , _con :: C f Text 
--       } deriving (Generic, Beamable)
-- type Ev = EvT 
-- type EvId = PrimaryKey EvT Identity 
-- instance Table EvT where 
--       data PrimaryKey EvT f = EvId (C f Text) deriving (Generic, Beamable)
--       primaryKey = EvId . _eid

-- data IdT f = Id {
--       _priv :: C f Text 
--       } deriving (Generic, Beamable)
-- type Id = IdT
-- type IdId = PrimaryKey IdT Identity
-- instance Table IdT where 
--       data PrimaryKey IdT f = IdId (C f Text) deriving (Generic, Beamable)
--       primaryKey = IdId . _priv

-- data RelayT f = Relay {
--       _relay :: C f Text
--       } deriving (Generic, Beamable)
-- type Relay = RelayT
-- type RelayId = PrimaryKey RelayT Identity 
-- instance Table RelayT where 
--       data PrimaryKey RelayT f = RelayId (C f Text) deriving (Generic, Beamable)
--       primaryKey = RelayId . _relay

-- data PlebT f = Pleb {
--           _pubp :: C f Text 
--         , _conp :: C f (Maybe Text)
--       } deriving (Generic, Beamable)
-- type Pleb = PlebT 
-- type PlebId = PrimaryKey PlebT Identity 
-- instance Table PlebT where 
--       data PrimaryKey PlebT f = PlebId (C f Text) deriving (Generic, Beamable)
--       primaryKey = PlebId . _pubp

-- data ReplyT f = Reply {
--         _idxr :: C f Int32
--       , _eidr :: PrimaryKey EvT f
--       , _eidrr :: (C f Text) -- Either (PrimaryKey EvT f) 
--       , _markerr :: C f (Maybe Marker)
--       } deriving (Generic, Beamable) 
-- type Reply = ReplyT
-- type ReplyId = PrimaryKey ReplyT Identity 
-- instance Table ReplyT where 
--       data PrimaryKey ReplyT f = ReplyId (C f Int32) deriving (Generic, Beamable)
--       primaryKey = ReplyId . _idxr

-- data MentionT f = Mention {
--         _idxm :: C f Int32
--       , _eidm :: PrimaryKey EvT f
--       , _pidm :: C f Text -- PrimaryKey PlebT f
--       } deriving (Generic, Beamable)
-- type Mention = MentionT
-- type MentionId = PrimaryKey MentionT Identity
-- instance Table MentionT where 
--       data PrimaryKey MentionT f = MentionId (C f Int32) deriving (Generic, Beamable)
--       primaryKey = MentionId . _idxm

-- instance HasDefaultSqlDataType Sqlite Marker where 
--     defaultSqlDataType _ _ _ = sqliteTextType 
-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Marker where 
--     sqlValueSyntax = autoSqlValueSyntax
-- instance FromBackendRow Sqlite Marker where
--     fromBackendRow = read . T.unpack <$> fromBackendRow 

-- gather :: [Either a b] ->  ([a], [b]) 
-- gather = foldr g ([],[]) 
--     where 
--     g (Right r) (mx, rx) = (mx, r:rx)  
--     g (Left l) (mx, rx) = (l:mx, rx)

-- reply :: ReplyT (QExpr Sqlite a)

-- reply = Reply default_ (val_ . EvId . wq $ i) (val_ . wq $ id) (val_ marker)

-- mention :: Hex32 -> Hex32 -> MentionT (QExpr Sqlite b) 

-- mention i id = Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id) 

-- insertEv :: Connection -> Event -> IO ()
-- insertEv conn e@(Event i s (Content{..})) = -- do 
--     -- ins <- E.try 
--     runBeamSqliteDebug print conn $ do
--         -- XXX run as a Transaction??

--         runInsert $ insertOnConflict (_plebs spec') 
--                                      (insertExpressions [Pleb (val_ $ wq pubkey) default_])
--                                       anyConflict
--                                       onConflictDoNothing
        
--         runInsert $ insert (_events spec') (insertValues [toEv e])
--         > flip map tags \case
--            -- ETag id _ marker -> runInsert $ insert (_replies spec') $ insertExpressions $ [
--            --       Reply default_ (val_ . EvId . wq $ i) (val_ . wq $ id) (val_ marker) :: _ 
--            --     ]
--            PTag id _ -> mention i id  
--                 -- runInsert $ insert (_mentions spec') $ insertExpressions $ [
--                  -- Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id) ]
--                 (i, id)
--            _ -> pure () 

--         -- (mx :: [MentionT (QExpr Sqlite r)] , rx :: [ReplyT (QExpr Sqlite y)]) 
--         -- let (mx, rx) = gather . catMaybes $ flip map tags \case
--         --         ETag id _ marker -> Just . Right $ reply
--         --         PTag id _ -> Just . Left $ mention
--         --         _ -> Nothing 
--         --         where 
--         --             reply :: ReplyT (QExpr Sqlite a)
--         --             reply = Reply default_ (val_ . EvId . wq $ i) (val_ . wq $ id) (val_ marker)
--         --             mention :: MentionT (QExpr Sqlite a) 
--         --             mention = Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id) 
                    
--         -- runInsert $ insert (_replies spec') $ insertExpressions $ rx

--         -- runInsert $ insert (_mentions spec') $ insertExpressions $ [
--         --     Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id) :: _ ]         

-- toEv :: Event -> EvT Identity 
-- toEv e = Ev 
--       (wq $ eid e) 
--       (PlebId . wq . pubkey . con $ e) 
--       (fromInteger . created_at . con $ e) 
--       (wq e)

-- wq :: ToJSON a => a -> Text 
-- wq = decodeUtf8 . BS.toStrict . encode 

-- insertId :: Connection -> Text -> IO ()
-- insertId conn privKey = runBeamSqlite conn $
--     runInsert $ insert (_identities spec') 
--               $ insertValues [Id privKey]

-- insertRelay :: Connection -> Text -> IO ()
-- insertRelay conn relayText = runBeamSqlite conn $ do
--     runInsert $ insert (_relays spec') 
--               $ insertValues [Relay relayText]

