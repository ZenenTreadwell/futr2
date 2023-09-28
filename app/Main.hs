
module Main (main) where

import Network.WebSockets 
import Control.Monad
import Database.SQLite.Simple as SQL
import Nostr.Beam
import Nostr.Relay
import Nostr.Harvest
import Nostr.Event
import Nostr.Boots
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM

main :: IO ()
main = do 
    o <- SQL.open "./futr.sqlite"
    f <- createDb o
    print "starting server on 9481"
    void . forkIO . runServer "127.0.0.1" 9481 
        $ acceptRequest >=> relay o f

    threadDelay 100000

    mapM_ (forkIO . runH . (\d -> harvestr o d)) 
        $ defaultRelay

    chan' <- atomically . dupTChan $ f
    forever $ atomically (readTChan chan') >>= 
        (print . ("e: " <>) . content . con)
    
    threadDelay maxBound
    
runH = \case 
    Just m -> m
    _ -> pure () 

