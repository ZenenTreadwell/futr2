module Futr.App where 

import Monomer 
import Nostr.Keys
import Nostr.Kinds
import Nostr.Pool
import Nostr.Event

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Database.SQLite.Simple

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel 
    deriving (Eq)
data AppEvent = 
      AppInit
    | SetCurrent Hex32
    | SetCurrentP Profile 

data Futr = Futr {
      pool :: Pool 
    , feed :: TChan Event
    , base :: Connection
    , top :: TChan AppEvent
    }

