
module Main (main) where 

import Prelude as P
import Control.Monad
import Data.Maybe
import Database.SQLite.Simple as SQL
import Data.Text.IO as TIO 
import System.Directory as D
import Data.Ini.Config
import Nostr.Event
import Nostr.Db.Create
import Nostr.Db.Insert
import Nostr.Relay
import Control.Concurrent
import Control.Concurrent.STM.TChan

import Database.Beam
import Database.Beam.Sqlite

import Nostr.Db.Schema
import Nostr.Db.Insert
import Nostr.Keys
import Nostr.Event

import Data.Aeson


main :: IO () 
main = do 
    d <- (<>"/.futr") <$> getHomeDirectory 
    createDirectoryIfMissing False d 
    let conf' = d <> "/futr.conf"
        db'   = d <> "/events.sqlite" 
    o <- SQL.open db' 
    f <- createDb o
    idz <- (runBeamSqlite o $ do    
        runSelectReturningList . select $ do 
            e <- all_ (_identities spec')
            pure (_priv $ e)
            )
            
    kp <- case idz of 
        (xnpub -> Just i ) : _ -> expandPriv i   

        _ -> do  
            newKp <- genKeyPair 
            runBeamSqlite o  
                $ runInsert (insert (_identities spec') 
                $ insertValues [Id (nsec newKp)])
            pure newKp       
    wr <- newTChanIO
    void . forkIO $ insertLoop wr
    localIdentity <- exportPub kp
    ctxt <- ("[d]\n" <>) . (<> "\n") <$> TIO.readFile conf' 
    case parseIniFile ctxt $ section "d" do 
                   p <- fieldMbOf "port" number
                   n <- fieldMb "name"
                   desc <- fieldMb "description"
                   c <- fieldMb "contact"
                   pk <- join . (qw <$>) <$> fieldMb "pubkey"
                   pure $ RC (fromMaybe "" n)
                             (fromMaybe "" desc)
                             (fromMaybe "" c)
                             (fromMaybe 9481 p)
                             (fromMaybe localIdentity pk)   

        of 
        Left err -> print ("config error: " <> conf') >> print err
        Right conf@(RC{rc_port}) -> do 
        
            P.putStrLn $ "nostr relay running on: " <> show rc_port 
            
            TIO.putStrLn . wq $ nip11 conf
             
            
            runRelay conf (wr, o) f  

