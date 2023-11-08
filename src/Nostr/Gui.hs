{-# LANGUAGE TypeSynonymInstances #-}

module Nostr.Gui where 

import Prelude as P
import Monomer
import Monomer.Core.Style
-- import Monomer.Lens
import Monomer.Hagrid
import Nostr.Event
import Nostr.Pool
import Control.Concurrent
-- import Control.Lens
import Nostr.Beam
import Nostr.Keys
import Database.SQLite.Simple as SQL
import Control.Monad
import Control.Concurrent.STM.TChan
import Control.Monad.STM
-- XXX uses lens
import Monomer.Widgets.Singles.TextArea
import Data.Map as M
import Data.Text as T
import Data.Text.Encoding as T
import Data.Maybe
import Text.URI

data AppEvent = 
      AppInit
    | SwitchTheme Theme
    | A Event
    | GetRe Hex32

data AppModel = B {
        theme :: Theme
      , msgs :: [Event]
      , pool :: Pool'
    } deriving (Eq)
    
mstart :: Pool' -> AppModel
mstart p = B lightTheme [] p

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _ m = vstack [
      label " under construction "
    , vstack $ P.map (label . decodeUtf8 . encodeUtf8. content . con) (msgs m)  
    , vstack $ P.map (label . render) (keys $ pool m)
    ]

handle
  :: SQL.Connection 
  -> TChan Event
  -> WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handle db f e n m x = case x of 
    AppInit -> [Producer (displayfeed f)]
    A e -> [Model $ m { msgs = e : (P.take 5 $ msgs m)}]
    GetRe i -> []
    SwitchTheme t -> [Model $ m { theme = t }]
    
displayfeed :: TChan Event -> (AppEvent -> IO ()) -> IO (  )
displayfeed f r = do 
    f' <- atomically . dupTChan $ f
    forever do 
        e <- atomically $ readTChan f'
        r . A $ e
    
config = [
        appWindowTitle "nostr"
      , appWindowIcon "./assets/images/icon.png"
      , appTheme darkTheme
      , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
      , appInitEvent AppInit
      ]

start :: SQL.Connection -> TChan Event -> Pool' -> IO ThreadId
start db ff pool = forkOS (startApp (mstart pool) (handle db ff) buildUI config)


