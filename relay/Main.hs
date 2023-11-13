
module Main (main) where 

import Control.Monad
import Data.Maybe
import Database.SQLite.Simple as SQL
import Data.Text.IO as TIO
import System.Directory as D
import Data.Ini.Config
import Nostr.Event
import Nostr.Db
import Nostr.Beam
import Nostr.Relay
import Nostr.Keys

main = do 
    d <- (<>"/.futr") <$> getHomeDirectory 
    createDirectoryIfMissing False d 
    let conf' = d <> "/futr.conf"
        db'   = d <> "/events.sqlite" 
    o <- SQL.open db' 
    f <- createDb o
    idents <- getIdentities o
    kp <- case idents of 
        [] -> genKeyPair >>= (\me -> (insertId o . un96 $ me) >> pure me)
        me : _ -> pure me
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
        Right conf -> runRelay conf o f  
