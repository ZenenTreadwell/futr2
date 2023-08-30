module Nostr.Relay where 

-- import Nostr.Db
import Prelude as P
import Control.Exception as E
import Network.WebSockets as WS
import Database.SQLite.Simple as SQL
import Data.Aeson as J
import Data.ByteString.Lazy as LB
import Control.Monad 
import Control.Concurrent
import Nostr.Wire
import qualified Data.ByteString.Base16 as Hex
import Nostr.Beam
import Nostr.Event 
import Nostr.Filter 

type Listen = IO (Either WS.ConnectionException LB.ByteString)

relay :: SQL.Connection -> ClientApp () 
relay db ws = forever do
    print "server"
    eo <- E.try . WS.receiveData $ ws :: Listen
    case decode <$> eo of 
        Right (Just d) -> case d of 
            Subscribe s fx -> do 
                ex <- fetchx db fx
                void $ WS.sendTextDatas ws $ P.map (encode . See s) ex   
                
            Submit e -> do 
                trust <- verifyE e 
                when trust (mask_ $ insertEv db e)  
            End s -> pure () --myThreadId >>= killThread 
        Right Nothing -> print . (<> " - nothing") . show $ eo
        Left z -> print z >> myThreadId >>= killThread 


fetchx :: SQL.Connection -> [Filter] -> IO [Event]
fetchx db fx = mconcat <$> mapM (fetch db) fx 

fetch :: SQL.Connection -> Filter -> IO [Event]
fetch _ _ = pure $ [wev, wev, wev, wev]





esig = Hex64 $ Hex.decodeLenient "908a15e46fb4d8675bab026fc230a0e3542bfade63da02d542fb78b2a8513fcd0092619a2c8c1221e581946e0191f2af505dfdf8657a414dbca329186f009262"
wev = Event evid esig ev 
ev = Content
    1
    [ 
      ETag evref Nothing Nothing 
    , PTag keyref Nothing  
    ] 
    "Walled gardens became prisons, and nostr is the first step towards tearing down the prison walls."         
    1673347337
    pub

evref = Hex32 $ Hex.decodeLenient "3da979448d9ba263864c4d6f14984c423a3838364ec255f03c7904b1ae77f206"

keyref = Hex32 $ Hex.decodeLenient "bf2376e17ba4ec269d10fcc996a4746b451152be9031fa48e74553dde5526bce"

pub = Hex32 $ Hex.decodeLenient "6e468422dfb74a5738702a8823b9b28168abab8655faacb6853cd0ee15deee93"

evid = Hex32 $ Hex.decodeLenient "4376c65d2f232afbe9b882a35baa4f6fe8667c4e684749af565f981833ed6a65"



