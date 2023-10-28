{-# Language
    DataKinds 
    , OverloadedStrings
#-}

module Main (main) where

import Prelude as P 
import qualified Data.ByteString.Base16 as Hex
import Network.WebSockets as WS
import Control.Monad
import Database.SQLite.Simple as SQL
import Nostr.Beam
import Nostr.Relay
import Nostr.Harvest
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
import Text.URI
import Data.Maybe
import Data.Traversable
import Data.Time.Clock.POSIX
import Nostr.Auth
import Data.Map as M 
import Control.Concurrent.STM.TVar
import Nostr.Pool
import Control.Exception as E
import System.IO.Error

main :: IO ()
main = do 
    o <- SQL.open "./futr.sqlite"
    f <- createDb o
    void . forkIO . run 9481 $ websocketsOr co 
        (acceptRequest >=> relay o f) 
        (serve x s) 
    threadDelay 100000

    idents <- getIdentities o
    me' <- case idents of 
        [] -> genKeyPair >>= (\me -> (insertId o . un96 $ me) >> pure me)
        me : _ -> pure me

    print "me pub"
    meep <- exportPub me' 
    print . wq $ meep

    print "me priv"
    print . wq $ me'

    tv <- newTVarIO M.empty

    let pool = Pool tv o me'

    sec :: Integer <- round <$> getPOSIXTime

    mapM_ (addRelay pool) defaultRelay
    castAll pool $ Subscribe "a" [ 
             liveF sec 
           , emptyF{ptagF=Just (PTagM [meep])}
           ]
    --     , emptyF{authorsF=Just (Authors ["5917"]) }
    --     ]

    -- threadDelay 1000000

    -- let pubgossip = Hex32 . Hex.decodeLenient $ 
    --                   "239bf80a7c0742bcc412b46ffba1975dec619592a011823071e3b491ce0338ae"
    -- example create and broadcast kind 1
    -- let kl = Content 1 [PTag pubgossip Nothing Nothing] "sigh nostr" sec 
    -- eee <- signE me' kl 
    -- castAll pool . Submit $ eee   

    -- example create and broadcast kind 4
    -- ee <- encryptE me' pubgossip "domino" 
    -- castAll pool . Submit $ ee   

    chan' <- atomically . dupTChan $ f
    void . forever $ atomically (readTChan chan') >>= \c -> do   
        print . ("e : "<>) . content . con $ c
        print . ("p : "<>) . wq . pubkey . con $ c
        x <- E.try $ if    kind (con c) == 4 
                        && getP (tags $ con c) == Just meep 
                     then decryptE me' c >>= print 
                     else pure () 
        case x of 
            Left (SomeException _) -> pure () 
            Right _ -> pure () 

getP :: [Tag] -> Maybe Hex32 
getP tx = case P.filter isPTag tx of 
    (PTag x _ _) : _ -> Just x   
    _ -> Nothing 
    where isPTag (PTag{}) = True 
          isPTag _ = False

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