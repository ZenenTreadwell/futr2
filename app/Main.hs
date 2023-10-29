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

    idents <- getIdentities o
    me' <- case idents of 
        [] -> genKeyPair >>= (\me -> (insertId o . un96 $ me) >> pure me)
        me : _ -> pure me

    print "me pub"
    meep <- exportPub me' 
    print . wq $ meep
    void . forkIO . run 9481 $ websocketsOr co 
        (acceptRequest >=> relay o f) 
        (serve (Proxy :: Proxy Nip11) (n11 meep)) 

    threadDelay 100000

    tv <- newTVarIO M.empty

    let pool = Pool tv o me'

    sec :: Integer <- round <$> getPOSIXTime

    mapM_ (addRelay pool) defaultRelay
    castAll pool $ Subscribe "a" [ 
             liveF sec 
           , emptyF{ptagF=Just (PTagM [meep])}
           ]

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


none :: Text
none = ""

type Nip11 = Get '[JSON] Text 
n11 :: Hex32 -> Server Nip11
n11 pub = return . decodeUtf8 . BS.toStrict . encode . object $ 
    -- XXX configurable ?  
    [ "name" .= none 
    , "description" .= none 
    , "pubkey" .= wq pub 
    , "contact" .= none
    , "supported_nips" .= 
         ([1, 2, 4, 9, 10, 45, 42, 40, 12, 16, 20, 33]
          :: [Int])
    , "software" .= ("http://github.com/autonomousorganization/futr2" :: Text) 
    , "version" .= ("0.0.0.0" :: Text)
    ]

co :: ConnectionOptions
co = defaultConnectionOptions