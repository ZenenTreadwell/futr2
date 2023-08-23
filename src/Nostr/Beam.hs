{-# LANGUAGE
      DeriveAnyClass 
    , StandaloneDeriving
    , TypeFamilies
    , UndecidableInstances 
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
import Data.Text.Encoding 
import Data.ByteString 
import Control.Monad.State
import Nostr.Event
import Data.Aeson

spec :: CheckedDatabaseSettings Sqlite Db
spec = defaultMigratableDbSettings 
spec' :: DatabaseSettings Sqlite Db
spec' = defaultDbSettings

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
      -- , _replies :: f (T ReplyT)
      , _mentions :: f (T MentionT)
      } deriving (Generic, Database Sqlite)

data EvT f = Ev {
        _eid :: C f Text 
      , _pub :: PrimaryKey PlebT f
      , _time :: C f Int64
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
      , _eidRR :: PrimaryKey EvT f
      , _markerR :: C f (Maybe Marker)
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

instance HasDefaultSqlDataType Sqlite Marker where 
    defaultSqlDataType _ _ _ = sqliteTextType 
instance HasSqlValueSyntax be String => HasSqlValueSyntax be Marker where 
    sqlValueSyntax = autoSqlValueSyntax
instance FromBackendRow Sqlite Marker where
    fromBackendRow = read . T.unpack <$> fromBackendRow 

insertEv :: Connection -> Event -> IO ()
insertEv conn e@(Event i s (Content{..})) = runBeamSqlite conn $ do
    runInsert $ insert (_plebs spec') $ insertValues [Pleb . wq $ pubkey]
    runInsert $ insert (_events spec') (insertValues [toEv e])
    let (m', r') = fromTags i tags
    insertMention m'
    -- insertReply r'
    where 
    insertMention :: [MentionT Identity] -> SqliteM ()
    insertMention mx -- idx eventEid pubKey = 
        = runInsert 
        $ insert (_mentions spec') 
        $ insertValues mx 
    
    -- insertReply :: [ReplyT Identity] -> SqliteM ()
    -- insertReply mx
    --     = runInsert 
    --     $ insert (_replies spec') 
    --     $ insertValues mx 

-- fromTags :: [Tag] -> ([MentionT Identity], [ReplyT Identity])
-- fromTags t = undefined 

type TagS = ([MentionT Identity], [ReplyT Identity])

-- fromTags :: [Tag] -> TagS 
fromTags eid tags = evalState runTags (tags, ([], []))
    where 

    runTags :: State ([Tag], TagS) TagS
       
    runTags = do 
        ( tx , b@(m', r') ) <- get
        case tx of 
            [] -> undefined -- pure b 
            t' : tx' -> case t' of
                ETag idx _ marker -> 
                    put ( tx' , ( 
                      m' 
                    , Reply 2 (EvId . wq $ eid) (EvId . wq $ idx) marker : r'
                    ))
                PTag idx _ -> 
                    put (tx, (
                        Mention 3 (EvId . wq $ eid) (PlebId . wq $ idx) : m'
                      , r'
                      ))
                Tag _ -> pure () 
        runTags 



insertId :: Connection -> Text -> IO ()
insertId conn privKey = runBeamSqlite conn $ do
    runInsert $ insert (_identities spec') $ insertValues [Id privKey]
    runInsert $ insert (_identities spec') $ insertValues [Id privKey]

insertRelay :: Connection -> Text -> IO ()
insertRelay conn relayText = runBeamSqlite conn $ do
    runInsert $ insert (_relays spec') 
              $ insertValues [Relay relayText]


toEv e = Ev 
      (wq $ eid e) 
      (PlebId . wq . pubkey . con $ e) 
      (fromInteger . created_at . con $ e) 
      (wq e)

wq :: ToJSON a => a -> Text 
wq = decodeUtf8 . toStrict . encode 
