
module Main (main) where

import Network.WebSockets 
import Data.Maybe 
import Text.URI --(URI)
import qualified Text.URI.QQ as QQ
import Control.Monad
import Control.Concurrent
import Database.SQLite.Simple as SQL
import Control.Concurrent.STM.TChan 
import Nostr.Beam
import Nostr.Harvest
import Nostr.Relay
import Nostr.Event

defaultRelay :: [URI] 
defaultRelay =  
    [ 
   -- [QQ.uri|ws://127.0.0.1:9481|]  
      [QQ.uri|ws://cvpcawhvxk27qvu5xrcblx7ingfxfivukesdpj7rwg63mflaly3tbuid.onion|]
    -- [QQ.uri|wss://nos.lol|]
    -- [QQ.uri|wss://relay.nostr.info|]
    -- , [QQ.uri|wss://relay.snort.social|]
    -- , [QQ.uri|wss://nostr-pub.wellorder.net|]
    -- , [QQ.uri|wss://nostr.oxtr.dev|]
    -- , [QQ.uri|wss://brb.io|]
    -- , [QQ.uri|wss://nostr-pub.semisol.dev|]
    -- , [QQ.uri|wss://nostr.zebedee.cloud|]
    -- , [QQ.uri|wss://relay.stoner.com|]
    -- , [QQ.uri|wss://relay.nostr.bg|]
    -- , [QQ.uri|wss://nostr-relay.untethr.me|]
    -- , [QQ.uri|wss://nostr.wine|]
    -- , [QQ.uri|wss://nostr.sandwich.farm|]
    -- , [QQ.uri|wss://nostr.rocks|] 
    -- , [QQ.uri|wss://relay.nostr.com.au|]
    -- , [QQ.uri|wss://nostrja-kari.heguro.com|]
    -- , [QQ.uri|wss://nostrja-kari-nip50.heguro.com|]
    -- , [QQ.uri|wss://purplepag.es|]
    ]

main :: IO ()
main = 
    let a :: SQL.Connection -> TChan Event -> ServerApp 
        a o f = acceptRequest >=> relay o f
    in do 
    o <- SQL.open "./futr.sqlite"
    f <- createDb o
    print . extractURI . head $ defaultRelay
    
    void . mapM forkIO . mapMaybe (harvestr o) $ defaultRelay 

    runServer "127.0.0.1" 9481 $ a o f


