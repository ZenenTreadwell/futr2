{-# LANGUAGE
      DeriveAnyClass 
    , StandaloneDeriving
    , TypeFamilies
    , UndecidableInstances 
    , PartialTypeSignatures
    #-}

module Nostr.Db where

import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Migrate
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite.Syntax
import Data.Int
import Data.Text (Text)
import Data.ByteString 
import qualified Data.Text as T 
import Nostr.Event
import Data.Time

spec :: CheckedDatabaseSettings Sqlite Db
spec = defaultMigratableDbSettings 

spec' :: DatabaseSettings Sqlite Db
spec' = defaultDbSettings

type T = TableEntity
data Db f = Db {
        _events :: f (T EvT)
      , _replies :: f (T ReplyT)
      , _mentions :: f (T MentionT)
      , _azt :: f (T AzRefT)
      , _azs :: f (T AzT)
      , _plebs :: f (T PlebT)
      , _dms :: f (T DmT)
      , _identities :: f (T IdT)
      , _relays :: f (T RelayT)
      } deriving (Generic, Database Sqlite)

data AzRefT f = AzRef {
        _tagzrid :: C f Int32
      , _azref :: PrimaryKey AzT f
      , _iieid :: PrimaryKey EvT f 
      } deriving (Generic, Beamable)
type AzRef = AzRefT
type AzRefId = PrimaryKey AzRefT Identity 
instance Table AzRefT where 
      data PrimaryKey AzRefT f = AzRefId (C f Int32) deriving (Generic, Beamable)
      primaryKey = AzRefId . _tagzrid 
      
data AzT f = Az {
        _idaz :: C f ByteString
      } deriving (Generic, Beamable)
type Az = AzT 
type AzId = PrimaryKey AzT Identity 
instance Table AzT where 
      data PrimaryKey AzT f = AzId (C f ByteString) deriving (Generic, Beamable)
      primaryKey = AzId . _idaz

data EvT f = Ev {
        _eid :: C f Text 
      , _pub :: PrimaryKey PlebT f
      , _time :: C f Int64
      , _kind :: C f Int32 
      , _expires :: C f (Maybe LocalTime)
      , _con :: C f Text
      } deriving (Generic, Beamable)
type Ev = EvT 
type EvId = PrimaryKey EvT Identity 
instance Table EvT where 
      data PrimaryKey EvT f = EvId (C f Text) deriving (Generic, Beamable)
      primaryKey = EvId . _eid

data IdT f = Id {
      _priv :: C f ByteString
      } deriving (Generic, Beamable)
type Id = IdT
type IdId = PrimaryKey IdT Identity
instance Table IdT where 
      data PrimaryKey IdT f = IdId (C f ByteString) deriving (Generic, Beamable)
      primaryKey = IdId . _priv

data RelayT f = Relay {
        _rid :: C f Int64
      , _uri :: C f Text
      , _actr :: C f Bool
      } deriving (Generic, Beamable)
type Relay = RelayT
type RelayId = PrimaryKey RelayT Identity 
instance Table RelayT where 
      data PrimaryKey RelayT f = RelayId (C f Int64) deriving (Generic, Beamable)
      primaryKey = RelayId . _rid

data PlebT f = Pleb {
          _pubp :: C f Text 
        , _conp :: C f (Maybe Text)
      } deriving (Generic, Beamable)
type Pleb = PlebT 
type PlebId = PrimaryKey PlebT Identity 
instance Table PlebT where 
      data PrimaryKey PlebT f = PlebId (C f Text) deriving (Generic, Beamable)
      primaryKey = PlebId . _pubp

data DmT f = Dm { 
        _dmid :: C f Int32
      , _dmcon :: C f ByteString
      , _dmdelivered :: C f Bool 
      , _dmrecipient :: PrimaryKey PlebT f 
      } deriving (Generic, Beamable)
type Dm = DmT 
type DmId = PrimaryKey DmT Identity
instance Table DmT where 
      data PrimaryKey DmT f = DmId (C f Int32) deriving (Generic, Beamable)
      primaryKey = DmId . _dmid

data ReplyT f = Reply {
        _idxr :: C f Int32
      , _eidr :: PrimaryKey EvT f
      , _eidrr :: C f Text
      , _markerr :: C f (Maybe Marker)
      } deriving (Generic, Beamable) 
type Reply = ReplyT
type ReplyId = PrimaryKey ReplyT Identity 
instance Table ReplyT where 
      data PrimaryKey ReplyT f = ReplyId (C f Int32) deriving (Generic, Beamable)
      primaryKey = ReplyId . _idxr

data MentionT f = Mention {
        _idxm :: C f Int32
      , _eidm :: PrimaryKey EvT f
      , _pidm :: C f Text -- PrimaryKey PlebT f
      } deriving (Generic, Beamable)
type Mention = MentionT
type MentionId = PrimaryKey MentionT Identity
instance Table MentionT where 
      data PrimaryKey MentionT f = MentionId (C f Int32) deriving (Generic, Beamable)
      primaryKey = MentionId . _idxm

instance HasDefaultSqlDataType Sqlite Marker where 
    defaultSqlDataType _ _ _ = sqliteTextType 

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Marker where 
    sqlValueSyntax = autoSqlValueSyntax

instance HasDefaultSqlDataType Sqlite Char where 
      defaultSqlDataType _ _ _ = sqliteTextType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Char where 
    sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite Marker where
    fromBackendRow = read . T.unpack <$> fromBackendRow 
