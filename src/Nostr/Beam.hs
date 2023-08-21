{-# LANGUAGE
      DeriveAnyClass 
    , StandaloneDeriving
    , TypeFamilies
    #-}
    -- DeriveGeneric 
    -- , FlexibleContexts
    -- , FlexibleInstances 
    -- , TypeSynonymInstances 
    -- , ImpredicativeTypes 
    -- , UndecidableInstances
    -- , MultiParamTypeClasses
module Nostr.Beam where

-- import Data.Aeson
-- import Data.Int
-- import Data.Text
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite.Syntax
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Data.Int
import Data.Text as T 

import Nostr.Event
import Data.Aeson

spec :: CheckedDatabaseSettings Sqlite Db
spec = defaultMigratableDbSettings 

createDb :: Connection -> IO () 
createDb conn = runBeamSqlite conn $ do 
   veri <- verifySchema migrationBackend spec
   _ <- checkSchema migrationBackend spec mempty
   case veri of 
       VerificationFailed _ -> autoMigrate migrationBackend spec
       VerificationSucceeded -> pure () 

type T = TableEntity
data Db f = Db {
        _events :: f (T EvT)
      , _identities :: f (T IdT)
      , _relays :: f (T RelayT)
      , _plebs :: f (T PlebT)
      , _replys :: f (T ReplyT)
      , _mentions :: f (T MentionT)
      } deriving (Generic, Database Sqlite)

data EvT f = Ev {
        _eid :: C f Text 
      , _pub :: PrimaryKey PlebT f
      , _con :: C f Text 
      } deriving (Generic, Beamable)
type Ev = EvT 
type EvId = PrimaryKey EvT Identity 
instance Table EvT where 
      data PrimaryKey EvT f = EvId (C f Text) deriving (Generic, Beamable)
      primaryKey = EvId . _eid

data IdT f = Id {
      _priv :: C f Text 
      } deriving (Generic, Beamable)
type Id = IdT
type IdId = PrimaryKey IdT Identity
instance Table IdT where 
      data PrimaryKey IdT f = IdId (C f Text) deriving (Generic, Beamable)
      primaryKey = IdId . _priv

data RelayT f = Relay {
      _relay :: C f Text
      } deriving (Generic, Beamable)
type Relay = RelayT
type RelayId = PrimaryKey RelayT Identity 
instance Table RelayT where 
      data PrimaryKey RelayT f = RelayId (C f Text) deriving (Generic, Beamable)
      primaryKey = RelayId . _relay

data PlebT f = Pleb {
        _pubp :: C f Text 
      } deriving (Generic, Beamable)
type Pleb = PlebT 
type PlebId = PrimaryKey PlebT Identity 
instance Table PlebT where 
      data PrimaryKey PlebT f = PlebId (C f Text) deriving (Generic, Beamable)
      primaryKey = PlebId . _pubp

data ReplyT f = Reply {
        _idxR :: C f Int32
      , _eidR :: PrimaryKey EvT f
      , _eidR' :: PrimaryKey EvT f
      } deriving (Generic, Beamable) 
type Reply = ReplyT
type ReplyId = PrimaryKey ReplyT Identity 
instance Table ReplyT where 
      data PrimaryKey ReplyT f = ReplyId (C f Int32) deriving (Generic, Beamable)
      primaryKey = ReplyId . _idxR

data MentionT f = Mention {
        _idxM :: C f Int32
      , _eidM :: PrimaryKey EvT f
      , _pidM :: PrimaryKey PlebT f
      } deriving (Generic, Beamable)
type Mention = MentionT
type MentionId = PrimaryKey MentionT Identity
instance Table MentionT where 
      data PrimaryKey MentionT f = MentionId (C f Int32) deriving (Generic, Beamable)
      primaryKey = MentionId . _idxM


      

      




-- instance HasDefaultSqlDataType Sqlite Hex96 where 
--       defaultSqlDataType _ _ _ = toJSON
      
-- createDb conn = runBeamSqlite conn $ do 
--    veri <- verifySchema migrationBackend spec
--    _ <- checkSchema migrationBackend spec mempty
--    case veri of 
--        VerificationFailed _ -> autoMigrate migrationBackend spec
--        VerificationSucceeded -> pure () 

-- data Db f = Db {
--       _events :: f (TableEntity _)
--     , _profiles :: f (TableEntity _)
--     , _identities :: f (TableEntity _)
--     } deriving (Generic, Database Sqlite) 
    
-- data HoldT f = Hold {
--       _held :: Columnar f (Maybe Text)
--     , _label :: Columnar f Text
--     , _status :: Columnar f HoldStatus
--     } deriving (Generic, Beamable) 
-- type Hold = HoldT Identity
-- type HoldId = PrimaryKey HoldT Identity

-- instance ToJSON (HoldT Identity) 

-- instance Table HoldT where
--     data PrimaryKey HoldT f = HoldId (Columnar f Text) deriving (Generic, Beamable)
--     primaryKey = HoldId . _label  






-- data HoldStatus = Created | Held | Pulled | Dropped
--                   deriving (Show, Read, Eq, Ord, Enum, Generic)
-- instance ToJSON HoldStatus

-- instance HasDefaultSqlDataType Sqlite HoldStatus where 
--     defaultSqlDataType _ _ _ = sqliteTextType 

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be HoldStatus where 
--     sqlValueSyntax = autoSqlValueSyntax
-- instance FromBackendRow Sqlite HoldStatus where
--     fromBackendRow = read . unpack <$> fromBackendRow


