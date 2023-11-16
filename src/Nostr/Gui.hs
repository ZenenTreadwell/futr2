-- {-# LANGUAGE TypeSynonymInstances #-}

module Nostr.Gui where 

import Prelude as P
import Monomer as O 
import Monomer.Core.Style
-- import Monomer.Lens
import Monomer.Hagrid
import Nostr.Event as N
import Nostr.Pool
import Nostr.Filter
import Control.Concurrent

import Control.Concurrent.STM.TVar
-- import Control.Lens
import Nostr.Beam
import Nostr.Keys
import Database.SQLite.Simple as SQL
import Control.Monad
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Monad.State
-- XXX uses lens
import Monomer.Widgets.Singles.TextArea
import Data.Map as M
import Data.Text as T
import Data.Text.Encoding as T
import Data.Maybe
import Text.URI
import Text.Regex.TDFA


reglinks :: Text 
reglinks = "http.+(jpg|gif|png)"

data AppEvent = 
      AppInit
    | FreshPool Pool'
    | ReModel AppModel
    | SwitchTheme Theme
    | A Event
    | GetRe Hex32

data AppModel = B {
        theme :: Theme
      , msgs :: [Event]
      , pool :: Pool'
      , relaycount :: Int
      , selectedeid :: Maybe Selecty
    } deriving (Eq)

data Selecty = Selecty Hex32 [Event] deriving (Eq)
    
mstart :: SQL.Connection -> Pool' -> AppModel
mstart o p = B lightTheme [] p (P.length p) Nothing

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _ m = vstack [
        label " under construction " `styleBasic` [textSize 30]
      , label " under construction "
      , label " under construction "
      , case selectedeid m of 
            Just (Selecty x ee) -> vstack $ 
                [ label $ wq x 
                , vstack (P.map showMsg ee) 
                ]  
            Nothing -> label "nothing"  
      , hstack [
          vstack $ P.map 
              showMsg
              (msgs m)  
        , vstack $ P.map (label . render) (keys $ pool m)
        ]
      ]
    
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
    A e -> case kind . con $ e of 
        1 -> [Model $ m { msgs = e : (P.take 5 $ msgs m)}]
        _ -> []
        
    GetRe i -> [Task $ do 
        eex <- fetch db (emptyF {etagF = Just (ETagM [i])})
        pure . ReModel $ m { selectedeid = Just (Selecty i eex) }
        ]
    SwitchTheme t -> [Model $ m { theme = t }]
    ReModel m -> [Model m]
    FreshPool p -> [ Task $ do 
        pure . ReModel $ m { pool = p }
        ]

showMsg :: Event -> WidgetNode AppModel AppEvent
showMsg (N.Event i _ (Content{..})) = box_ [onClick (GetRe i)] $ 
            case evalState extractlinks ("", [], content)  of 
                (b4, lt, af) -> vstack [
                      label_ (mymultiline b4) labelconfig
                    , hstack $ P.map (flip image_ [fitWidth, fitHeight]) lt
                    ]
extractlinks :: State (Text, [Text], Text) (Text, [Text], Text)
extractlinks = do 
    (tb, tlx, ta) <- get  
    case ta =~ reglinks :: (Text, Text, Text) of 
        (t, "", "") -> pure (tb <> t, tlx, "")
        (blt, ll, btl) -> put (tb <> blt, ll : tlx, btl) >> extractlinks
    
        
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

labelconfig :: [LabelCfg AppModel AppEvent]
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


