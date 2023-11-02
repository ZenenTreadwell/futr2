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
import Control.Monad.STM
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Data.Proxy
import Data.Text 
import Data.Text.Encoding
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

main :: IO ()
main = do 
    o <- SQL.open "./futr.sqlite"
    f <- createDb o
    forkIO $ runRelay 9481 o f 

    void $ start o f 
    idents <- getIdentities o
    k <- case idents of 
        [] -> genKeyPair >>= (\me -> (insertId o . un96 $ me) >> pure me)
        me : _ -> pure me
    u <- exportPub k
    p <- poolParty

    let pool :: Pool = p o k 
    sec :: Integer <- round <$> getPOSIXTime
    mapM_ (addRelay pool) defaultRelay
    castAll pool $ Subscribe "a" [ 
             liveF sec 
           , emptyF{ptagF=Just (PTagM [u])}
           ]

    threadDelay maxBound

