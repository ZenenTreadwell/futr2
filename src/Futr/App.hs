module Futr.App where 

import Monomer 
import Nostr.Keys
import Nostr.Kinds
import Nostr.Pool
import Nostr.Event
import Nostr.Filter
import Nostr.Beam 

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Database.SQLite.Simple

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent

data AppModel = AppModel {
      current :: Event
    , children :: [Event]
    -- , myProfile :: Maybe Profile  
    , showMode :: AppPage  
    }  deriving (Eq)

data AppPage = Doge | Uni | Bull | ImgFeed deriving Eq


data AppEvent = 
      AppInit
    | SetCurrent Hex32
    | ApplyCurrent (Maybe (Event, [Event]) ) 
    | SetPage AppPage

data Futr = Futr {
      pool :: Pool 
    , feed :: TChan Event
    , base :: Connection
    , top :: TChan AppEvent
    }

appHandle :: Futr -> AppEventHandler AppModel AppEvent
appHandle futr@(Futr{base, top}) _ _ model event = case event of
    AppInit -> let toppy r = forever $ atomically (readTChan top) >>= r 
               in [Producer toppy]
    SetCurrent x -> [Task $ do 
        print "**************************"
        ApplyCurrent <$> fetchHex futr x ]
    ApplyCurrent (Just (e, ex)) -> [Model $ model{current=e, children=ex}]
    SetPage p -> [Model (model{showMode=p})]
    _ -> []

fetchHex :: Futr -> Hex32 -> IO (Maybe (Event, [Event])) 
fetchHex (Futr{base}) x = do 
    print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"  
    frompub <- fetch base emptyF{
                              authorsF=Just (Authors [wq x]) 
                            , kindsF=Just (Kinds [0])
                            } 
    case frompub of 
        [] -> do 
            e2 <- fetch base emptyF{idsF=Just (Ids [wq x])} 
            case e2 of 
                [] -> pure Nothing 
                e3 : _ -> Just . (e3,) <$> fetch base emptyF{etagF=Just (ETagM [x])}
        e : _ -> Just . (e,) 
                      <$> fetch base emptyF{authorsF=Just (Authors [wq x])} 
                    
