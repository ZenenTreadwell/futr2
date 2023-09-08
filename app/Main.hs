
module Main (main) where

import Network.WebSockets 
import Data.Maybe 
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Database.SQLite.Simple as SQL
import Nostr.Beam
import Nostr.Harvest
import Nostr.Relay
import Nostr.Boots

main :: IO ()
main = do 
    o <- SQL.open "./futr.sqlite"
    f <- createDb o
    void . mapM forkIO . mapMaybe (harvestr o) $ defaultRelay 
    (runServer "127.0.0.1" 9481 $ acceptRequest >=> relay o f)
    
