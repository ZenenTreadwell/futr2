module Nostr.Db.Create where 

import Nostr.Event
import Nostr.Db.Schema

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Database.SQLite.Simple.Function
import Database.Beam.Sqlite.Migrate
import Database.Beam.Migrate.Generics

import Database.Beam.Migrate.Simple
import Control.Concurrent.STM.TChan
import Data.Text (Text)

import Control.Monad.STM

spec :: CheckedDatabaseSettings Sqlite Db
spec = defaultMigratableDbSettings 

createDb :: Connection -> IO (TChan Event) 
createDb o = do 
    _ <- runBeamSqlite o $ do 
        veri <- verifySchema migrationBackend spec
        _ <- checkSchema migrationBackend spec mempty
        case veri of 
            VerificationFailed _ -> autoMigrate migrationBackend spec
            VerificationSucceeded -> pure () 
        veri2 <- verifySchema migrationBackend spec
        case veri2 of 
            VerificationFailed e -> error . show $ e
            VerificationSucceeded -> pure () 
        
    _ <- execute_ o "CREATE TRIGGER IF NOT EXISTS updaterhook AFTER INSERT ON events BEGIN SELECT eventfeed(NEW.con); END;"
    chan <- newTChanIO
    _ <- createFunction o "eventfeed" (eventfeed chan) 
    pure chan

eventfeed :: TChan Event -> Text -> IO Text  
eventfeed chan t = case qw t of 
    Just e -> atomically $ writeTChan chan e
    _ -> pure () 
    >> pure "1"
