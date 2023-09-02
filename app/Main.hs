
module Main (main) where

import Network.WebSockets 
import Text.URI --(URI)
import qualified Text.URI.QQ as QQ
import Control.Monad
import Control.Concurrent
import Database.SQLite.Simple as SQL
import Nostr.Beam
import Nostr.Harvest
import Nostr.Relay

-- import Nostr.Event
defaultRelay :: [URI] --_ -- m [URI] 
defaultRelay =  
    [ 
        [QQ.uri|ws://127.0.0.1:9487|]  
    --   , [QQ.uri|wss://nos.lol|]
    -- , [QQ.uri|wss://relay.nostr.info|]
    -- , [QQ.uri|wss://relay.snort.social|]
    -- , [QQ.uri|wss://nostr-pub.wellorder.net|]
    -- , [QQ.uri|wss://nostr.oxtr.dev|]
    -- , [QQ.uri|wss://brb.io|]
    -- , [QQ.uri|wss://nostr-pub.semisol.dev|]
    -- , [QQ.uri|wss://nostr.zebedee.cloud|]
    -- , [QQ.uri|wss://relay.stoner.com|]
    -- , [QQ.uri|wss://relay.nostr.bg|]
    -- , [QQ.uri|wss://nostr-relay.untethr.me|]
    , [QQ.uri|wss://nostr.wine|]
    -- , [QQ.uri|wss://nostr.sandwich.farm|]
    -- , [QQ.uri|wss://nostr.rocks|] 
    -- , [QQ.uri|wss://relay.nostr.com.au|]
    -- , [QQ.uri|wss://nostrja-kari.heguro.com|]
    -- , [QQ.uri|wss://nostrja-kari-nip50.heguro.com|]
    ]

main :: IO ()
main = do 
    o <- open "./futr.sqlite"
    createDb o
    
    void $ flip mapM defaultRelay $  
        forkIO . harvestRelay o   

    runServer "127.0.0.1" 9487 \p -> 
        acceptRequest p >>= relay o 

    -- threadDelay maxBound
    -- pure ()





