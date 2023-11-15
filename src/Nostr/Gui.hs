-- {-# LANGUAGE TypeSynonymInstances #-}

module Nostr.Gui where 

import Prelude as P
import Monomer as O 
import Monomer.Core.Style
-- import Monomer.Lens
import Monomer.Hagrid
import Nostr.Event
import Nostr.Pool
import Control.Concurrent

import Control.Concurrent.STM.TVar
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
import Text.Regex.TDFA

data AppEvent = 
      AppInit
    | FreshPool Pool'
    | SwitchTheme Theme
    | A Event
    | GetRe Hex32

data AppModel = B {
        theme :: Theme
      , msgs :: [Event]
      , pool :: Pool'
      , relaycount :: Int
      , selectedeid :: Maybe Hex32
    } deriving (Eq)
    
mstart :: SQL.Connection -> Pool' -> AppModel
mstart o p = B lightTheme [] p (P.length p) Nothing

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _ m = vstack [
        label " under construction "
      , label " under construction "
      , label " under construction "
      , case selectedeid m of 
            Just x -> label $ wq x
            Nothing -> label "nothing"  
      , hstack [
          vstack $ P.map 
              (flip label_ labelconfig . decodeUtf8 . encodeUtf8 . mymultiline . content . con) 
              (msgs m)  
        , vstack $ P.map (label . render) (keys $ pool m)
        ]]

handle
  :: SQL.Connection 
  -> TChan Event
  -> Pool
  -> WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handle db f pool e n m x = case x of 
    AppInit -> [Producer (fresher pool), Producer (displayfeed f)]
    A e -> [Model $ m { msgs = e : (P.take 5 $ msgs m)}]
    GetRe i -> []
    SwitchTheme t -> [Model $ m { theme = t }]
    FreshPool p -> [ Model $ m { pool = p }]
    
fresher :: Pool -> (AppEvent -> IO ()) -> IO () 
fresher pool@(Pool p _ _) r = do 
    threadDelay 5000000
    readTVarIO p >>= r . FreshPool  
    fresher pool r 
        
displayfeed :: TChan Event -> (AppEvent -> IO ()) -> IO (  )
displayfeed f r = do 
    f' <- atomically . dupTChan $ f
    forever do 
        e <- atomically $ readTChan f'
        r . A $ e
    

mymultiline :: Text -> Text 
mymultiline t = case T.splitAt 42 t of 
    ((<> "\n") -> t, T.null -> True) -> t 
    ((<> "\n") -> t1 , t2) -> t1 <> (mymultiline t2) 

labelconfig = [ O.multiline ]
    
config = [
     appWindowTitle "nostr"
   , appWindowIcon "./assets/images/icon.png"
   , appTheme darkTheme
   , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
   , appInitEvent AppInit
   ]

start :: SQL.Connection -> TChan Event -> Pool -> IO ThreadId
start db ff pool@(Pool v _ _) = do 
    p' <- readTVarIO v 
    forkOS $
      startApp 
        (mstart db p') 
        (handle db ff pool) 
        buildUI 
        config


