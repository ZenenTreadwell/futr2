{-# LANGUAGE TypeSynonymInstances #-}

module Nostr.Gui where 

import Monomer
import Monomer.Core.Style
-- import Monomer.Lens
import Monomer.Hagrid
import Nostr.Event
import Control.Concurrent
-- import Control.Lens
import Nostr.Beam
import Nostr.Keys
import Database.SQLite.Simple as SQL
import Control.Monad
import Control.Concurrent.STM.TChan
import Control.Monad.STM

data AppEvent = 
      AppInit
    | SwitchTheme Theme
    | A Event
    | GetRe Hex32

data AppModel = B {
        theme :: Theme
      , msgs :: [Event]
    } deriving (Eq)
    
mstart :: AppModel
mstart = B lightTheme []

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _ m = vstack [
      label "test"
    , vstack $ flip map (msgs m) (\e -> label (content . con $ e)) 
        
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
    A e -> [Model $ m { msgs = e : (msgs m)}]
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

start :: SQL.Connection -> TChan Event -> IO ThreadId
start db ff = forkOS (startApp mstart (handle db ff) buildUI config)


