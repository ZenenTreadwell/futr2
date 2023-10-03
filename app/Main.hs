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
import Network.Wai.Handler.WebSockets

main :: IO ()
main = do 
    o <- SQL.open "./futr.sqlite"
    f <- createDb o
    void . forkIO . run 9481 $ websocketsOr co 
        (acceptRequest >=> relay o f) 
        (serve x s) 
    threadDelay 100000
    mapM_ (forkIO . runH . (\d -> harvestr o d)) $ defaultRelay
    chan' <- atomically . dupTChan $ f
    void . forever $ atomically (readTChan chan') >>= \c -> do   
        -- print . ("e :"<>) . content . con
        mapM print . tags . con $ c 
        print . ("e : "<>) . content . con $ c
    threadDelay maxBound
    
runH = \case 
    Just m -> m
    _ -> pure () 

type Nip45 = Get '[JSON] Text 
x :: Proxy Nip45
x = Proxy
s :: Server Nip45
s = return . decodeUtf8 . BS.toStrict . encode . object $ 
    [ "name" .= (""::Text) 
    , "description" .= (""::Text) 
    , "pubkey" .= (""::Text) 
    -- XXX configurable 
    ]

co :: ConnectionOptions
co = defaultConnectionOptions