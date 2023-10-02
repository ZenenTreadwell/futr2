{-# Language
    DataKinds 
    , OverloadedStrings
#-}

module Main (main) where

import Network.WebSockets as WS
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
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Data.Proxy
import Data.Text 
import Data.Text.Encoding
import Data.Aeson
import Data.ByteString as BS

main :: IO ()
main = do 
    forkIO . run 9481 $ serve x s 
    o <- SQL.open "./futr.sqlite"
    f <- createDb o
    print "starting server on 9481"
    void . forkIO . WS.runServer "127.0.0.1" 9481 
        $ acceptRequest >=> relay o f
    threadDelay 100000
    mapM_ (forkIO . runH . (\d -> harvestr o d)) 
        $ defaultRelay
    chan' <- atomically . dupTChan $ f
    forever $ atomically (readTChan chan') >>= 
        (mapM print . tags . con)
    
    threadDelay maxBound
    
runH = \case 
    Just m -> m
    _ -> pure () 

type Nip45 = Get '[JSON] Text 

s :: Server Nip45
s = return . decodeUtf8 . BS.toStrict . encode . object $ 
    [ "name" .= ("" :: Text) 
    , "description" .= (""::Text) 
    , "pubkey" .= (""::Text) 
    -- XXX configurable 
    ]

x :: Proxy Nip45
x = Proxy
    

    