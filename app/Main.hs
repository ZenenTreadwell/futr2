{-# Language
    DataKinds 
    , OverloadedStrings
#-}

module Main (main) where

import Prelude as P 
import Network.WebSockets as WS
import Control.Monad
import Database.SQLite.Simple as SQL
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.Monad.STM
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Data.Proxy
import Data.Text as T
import Data.Text.Encoding
import Data.Text.IO as TIO
import Data.Maybe
import Data.Aeson
import Data.ByteString as BS
import Network.Wai.Handler.WebSockets
import Nostr.Keys
import Nostr.Wire
import Data.Time.Clock.POSIX
import Data.Map as M 
import Control.Concurrent.STM.TVar
import Control.Exception as E
import System.Directory as D
import System.IO as S
import Data.Ini.Config
import Nostr.Beam
import Nostr.Relay
import Nostr.Event
import Nostr.Boots
import Nostr.Direct
import Nostr.Filter
import Nostr.Pool
import Futr.Gui 

main :: IO ()
main = do 
    d <- (<>"/.futr") <$> getHomeDirectory 
    createDirectoryIfMissing False d 
    let conf' = d <> "/futr.conf"
        db'   = d <> "/events.sqlite" 
    o <- SQL.open db' 
    f <- createDb o
  
    idents <- getIdentities o
    kp <- case idents of 
        [] -> genKeyPair >>= (\me -> 
                 (insertId o . un96 $ me) 
                   >> pure me)
        me : _ -> pure me
    localIdentity <- exportPub kp
    
    sd <- doesFileExist conf' 
    if sd then pure () 
          else S.writeFile conf' "#"
    
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
        Right conf ->  do 
            print conf
            void . forkIO $ runRelay conf o f  
            pool <- poolParty o kp
            start o f pool

