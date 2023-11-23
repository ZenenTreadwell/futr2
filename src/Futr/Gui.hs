
module Futr.Gui where 

import Prelude as P
import Monomer as O 
import Data.ByteString.Lazy as LB
import Monomer.Core.Style
import Monomer.Hagrid
import Nostr.Event as N
import Nostr.Pool
import Nostr.Filter
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Nostr.Beam
import Nostr.Keys
import Database.SQLite.Simple as SQL
import Control.Monad
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Monad.State
import Monomer.Widgets.Singles.TextArea
import Monomer.Widgets.Singles.TextField
import Monomer.Widgets.Singles.SeparatorLine
import Monomer.Widgets.Singles.NumericField
import Data.Map as M
import Data.Text as T
import Data.Text.Encoding as T
import Data.Maybe
import Text.URI
import Text.Regex.TDFA
import Monomer.Widgets.Singles.Base.InputField
import Data.Aeson as J

reglinks :: Text 
reglinks = "http.+(jpg|gif|png)"

data AppEvent = 
      AppInit
    | Mode AppMode 
    | Wtf Text 
    | Wth Int
    | Entuh 
    | FreshPool Pool'
    | ReModel AppModel
    | SwitchTheme Theme
    | A Event
    | GetRe Hex32
    | NextImg

data AppModel = AppModel {
        theme :: Theme -- XXX 
      , mode :: AppMode      
      , msgs :: [Event]
      , imgs :: [Text]
      , pool :: Pool'
      , texts :: Text
      , selectedeid :: Maybe Selecty
    } deriving (Eq)

data AppMode = Doge | Unicorn | Bull deriving (Eq, Show)

    
data Selecty = Selecty Hex32 [Event] deriving (Eq, Show)
    
mstart :: SQL.Connection -> Pool' -> AppModel
mstart o p = AppModel darkTheme Unicorn [] [] p "futr" Nothing

buildUI
  :: SQL.Connection
  -> WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI db _ m = themeSwitch (theme m) . keystroke [("Enter", Entuh)] $ vstack [
      box_ [expandContent] $ hstack [ 
          box_ [onClick (Mode Doge), alignLeft] $ label "doge" 
        , box_ [onClick (Mode Unicorn), alignCenter] $ 
            label " under construction " `styleBasic` [textSize 30]
        , box_ [onClick (Mode Bull), alignRight] $ label "bull" 
        ]
        -- , textFieldV (texts m) Wtf 
    , box_ [onClick (NextImg)] . label . T.pack . P.show 
             $ (P.length (imgs m)) 
    , hsplit (
          box_ [onClick (NextImg)] case imgs m of 
            []    -> separatorLine
            t : _ -> image_ t [fitEither]
        , vscroll . vstack $ P.map (box_ [alignRight]) $ [ 
             vstack $ (P.map (showMsg) (msgs m))
           ]
        
      ) `nodeVisible` (mode m == Unicorn)
    , vstack  (P.map (label . render) (keys $ pool m))
            `nodeVisible` (mode m == Bull)
    , label "doges" `nodeVisible` (mode m == Doge)      
    
    ]
        -- , case selectedeid m of 
        --       Just (Selecty x ee) -> vstack $ 
        --           [ label $ wq x 
        --           , vstack (P.map showMsg ee) 
        --           ]  
        --       Nothing -> label "nothing"  

ofof :: Map Hex32 Object -> Hex32 -> WidgetNode AppModel AppEvent 
ofof mp i = case M.lookup i mp of 
    Just o -> (flip label_ labelconfig) . wq $ o
    Nothing -> label "nothing"
 


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
    Mode moo -> [Model m {mode=moo}]
    Wtf t -> [ Model m {texts = t} ]
    Entuh -> [ Model m {texts=""} ]
    A e -> case kind . con $ e of 
        1 -> [Task $ do 
            let (strp, imgx, _) = evalState extractlinks ("", [], (content . con) e)
            
            pure . ReModel $ m { 
                  msgs = e : (P.take 5 $ msgs m)
                , imgs = (imgs m) <> imgx
                }]
        _ -> []
    NextImg -> [Model $ m { imgs = (P.drop 1 (imgs m)) }]
    GetRe i -> [Task $ do 
        pure . ReModel $ m { selectedeid = Just (Selecty i []) }
        ]
    SwitchTheme t -> [Model $ m { theme = t }]
    ReModel m -> [Model m]
    FreshPool p -> [ Task $ do 
        zeroes <- fetch db emptyF { kindsF = Just (Kinds [0]) } 
        -- let pleap = P.foldr 
        --                 (uncurry M.insert) 
        --                 (plebs m) 
        --                 (catMaybes $ P.map getr zeroes) 
        selly <- case selectedeid m of 
            Just (Selecty i _) -> Just . Selecty i 
                <$> fetch db (emptyF {etagF = Just (ETagM [i])})
            Nothing -> pure Nothing
        pure . ReModel $ m { 
              selectedeid = selly
            , pool = p }
        ]

getr :: Event -> Maybe (Hex32, Object)
getr (N.Event _ _ (Content{pubkey, content})) = 
    (pubkey,) <$> byObjr content

byObjr :: Text -> Maybe Object 
byObjr (encodeUtf8 -> t) = case decode . LB.fromStrict $ t of 
    Just (Object o) -> Just o
    _ -> Nothing
    
showMsg :: Event -> WidgetNode AppModel AppEvent
showMsg (N.Event i _ (Content{..})) = box_ [onClick (GetRe i)] $ 
    case evalState extractlinks ("", [], content)  of 
        (b4, lt, af) -> label_ b4 labelconfig

extractlinks :: State (Text, [Text], Text) (Text, [Text], Text)
extractlinks = do 
    (tb, tlx, ta) <- get  
    case ta =~ reglinks :: (Text, Text, Text) of 
        (t, "", "") -> pure (tb <> t, tlx, "")
        (t, ll, "") -> pure (tb <> t, ll : tlx, "")
        (blt, ll, btl) -> put (tb <> blt, ll : tlx, btl) >> extractlinks
    
        
fresher :: Pool -> (AppEvent -> IO ()) -> IO () 
fresher pool@(Pool p _ _) r = do 
    threadDelay 5000000
    readTVarIO p >>= r . FreshPool  
    r NextImg
    fresher pool r 
        
displayfeed :: TChan Event -> (AppEvent -> IO ()) -> IO (  )
displayfeed f r = do 
    f' <- atomically . dupTChan $ f
    forever do 
        e <- atomically $ readTChan f'
        r . A $ e
    
labelconfig :: [LabelCfg AppModel AppEvent]
labelconfig = [ O.multiline , trimSpaces]
    

start :: SQL.Connection -> TChan Event -> Pool -> IO ThreadId
start db ff pool@(Pool v _ _) = do 
    p' <- readTVarIO v 
    forkOS $
      startApp 
        (mstart db p') 
        (handle db ff pool) 
        (buildUI db) 
        [ appWindowTitle "nostr"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
        , appInitEvent AppInit
        ]

