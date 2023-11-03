{-# Language
    DataKinds 
    , OverloadedStrings
#-}

module Main (main) where

import Prelude as P 
import Network.WebSockets as WS
import Control.Monad
import Database.SQLite.Simple as SQL
import Nostr.Beam
import Nostr.Relay
import Nostr.Event
import Nostr.Boots
import Nostr.Direct
import Nostr.Filter
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Control.Monad.STM
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Data.Proxy
import Data.Text 
import Data.Text.Encoding
import Data.Text.IO as TIO
import Data.Aeson
import Data.ByteString as BS
import Network.Wai.Handler.WebSockets
import Nostr.Keys
import Nostr.Wire
import Data.Time.Clock.POSIX
import Data.Map as M 
import Control.Concurrent.STM.TVar
import Nostr.Pool
import Control.Exception as E
import Nostr.Gui
import System.Directory as D
import System.IO as S
import Data.Ini.Config





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
        [] -> genKeyPair >>= (\me -> (insertId o . un96 $ me) >> pure me)
        me : _ -> pure me
    localIdentity <- exportPub kp
    
    sd <- doesFileExist conf' 
    if sd then pure () 
          else S.writeFile conf' "#"
    
    ctxt <- TIO.readFile conf' 
    let conf = parseIniFile ctxt $ sectionMb "fuck" do 
                   p <- fieldMbOf "port" number
                   n <- fieldMb "name"
                   d <- fieldMb "description"
                   c <- fieldMb "contact"
                   pk <- join . (qw <$>) <$> fieldMb "pubkey"
                   pure $ RC (maybe "" id n)
                             (maybe "" id d)
                             (maybe "" id c)
                             (maybe 9481 id p)
                             (maybe localIdentity id pk)   

    print conf
        
    -- void . forkIO $ runRelay conf o f  
    

    -- void (start o f)
    
    -- forkIO $ 
    --     p <- poolParty

    --     let pool :: Pool = p o k 
    --     sec :: Integer <- round <$> getPOSIXTime
    --     mapM_ (addRelay pool) defaultRelay
    --     castAll pool $ Subscribe "a" [ 
    --              liveF sec 
    --            , emptyF{ptagF=Just (PTagM [u])}
    --            ]

    -- threadDelay maxBound

