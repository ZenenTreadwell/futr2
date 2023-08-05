{-# LANGUAGE 
      DuplicateRecordFields
    , NamedFieldPuns 
    , DeriveGeneric 
    , DeriveAnyClass 
    , StandaloneDeriving
    , TypeSynonymInstances 
    , TypeFamilies
    , FlexibleInstances 
    , FlexibleContexts
    , ImpredicativeTypes 
    , UndecidableInstances
    , MultiParamTypeClasses
    #-}

module Base where

-- import Data.Aeson
-- import Data.Int
-- import Data.Text
-- import Database.Beam
-- import Database.Beam.Sqlite
-- import Database.SQLite.Simple
-- import Database.Beam.Migrate
-- import Database.Beam.Migrate.Simple
-- import Database.Beam.Backend.SQL
-- import Database.Beam.Sqlite.Syntax
-- import Database.Beam.Sqlite.Migrate (migrationBackend)
    
-- -- settings :: DatabaseSettings be HoldDb
-- settings = defaultDbSettings

-- spec :: CheckedDatabaseSettings Sqlite Db
-- spec = defaultMigratableDbSettings

-- createDb conn = runBeamSqlite conn $ do 
--    veri <- verifySchema migrationBackend spec
--    _ <- checkSchema migrationBackend spec mempty
--    case veri of 
--        VerificationFailed _ -> autoMigrate migrationBackend spec
--        VerificationSucceeded -> pure () 

-- data Db f = Db {
--       _events :: f (TableEntity _)
--     , _tags :: f (TableEntity _)
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


