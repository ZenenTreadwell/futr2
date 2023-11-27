
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
import Nostr.Kinds
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

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel {
        theme :: Theme 
      , mode :: AppMode      
      , msgs :: [Event]
      , imgs :: [Text]
      , pool :: Pool'
      , texts :: Text
      , selectedeid :: Maybe Selecty
    } deriving (Eq)
data AppMode = Doge | Unicorn | Bull deriving (Eq, Show)

data Selecty = Selecty Hex32 [Event] deriving (Eq, Show)

data AppEvent = 
      AppInit
    | ReModel AppModel
    | TextField Text
    | Mode AppMode 
    | FreshPool Pool'
    | SwitchTheme Theme
    | A Event
    | GetRe Hex32
    | NextImg (Maybe Int)

handle :: SQL.Connection  -> TChan Event -> Pool -> AppEnv -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handle db f (Pool p _ _) _ _ m x = case x of 
    AppInit -> [Producer fresher, Producer displayfeed]
        where 
        fresher :: (AppEvent -> IO ()) -> IO () 
        fresher r = do 
            threadDelay 5000000
            readTVarIO p >>= r . FreshPool  
            fresher r 
                
        displayfeed :: (AppEvent -> IO ()) -> IO (  )
        displayfeed r = do 
            f' <- atomically . dupTChan $ f
            forever do 
                e <- atomically $ readTChan f'
                r . A $ e
                    
    ReModel n -> [Model n]
    Mode moo -> [Model m {mode=moo}]
    TextField t -> [ Model m {texts = t} ]
    A e -> case kind . con $ e of 
        0 -> [Model m { msgs = e : msgs m }]
        1 -> [Task $ do 
            let (_, imgx, _) = evalState extractlinks ("", [], (content . con) e)
            pure . ReModel $ m { 
                  msgs = e : (P.take 5 $ msgs m)
                , imgs = (imgs m) <> imgx
                }]
        _ -> []
    NextImg (fromMaybe 1 -> i) -> [Model $ m { imgs = (P.drop i (imgs m)) }]
    GetRe i -> [Task $ do 
        pure . ReModel $ m { selectedeid = Just (Selecty i []) }
        ]
    SwitchTheme t -> [Model $ m { theme = t }]
    FreshPool p -> [ Task $ do 
        -- zeroes <- fetch db emptyF { kindsF = Just (Kinds [0]) } 
        selly <- case selectedeid m of 
            Just (Selecty i _) -> Just . Selecty i 
                <$> fetch db (emptyF {etagF = Just (ETagM [i])})
            Nothing -> pure Nothing
        pure . ReModel $ m { 
              selectedeid = selly
            , pool = p }
        ]
        
    
mstart :: SQL.Connection -> Pool' -> AppModel
mstart o p = AppModel darkTheme Unicorn [] [] p "futr" Nothing

buildUI :: AppEnv  -> AppModel -> AppNode
buildUI _ m = 
    themeSwitch (theme m) -- . keystroke [("Enter", Entuh)] 
    $ vstack [
      box_ [expandContent] $ hstack [ 
          box_ [onClick (Mode Doge), alignLeft] $ label "doge" 
        , box_ [onClick (Mode Unicorn), alignCenter] $ 
            label " under construction " `styleBasic` [textSize 30]
        , box_ [onClick (Mode Bull), alignRight] $ label "bull" 
        ]
    , textFieldV (texts m) TextField 
    , hsplit (
          case imgs m of 
            []    -> spacer
            (tt : []) -> box_ [onClick (NextImg Nothing)] . vstack $ [ 
                  image_ tt [fitWidth] 
                , spacer
                , separatorLine 
                ]
            (tt : ((P.take 5) . (P.zipWith (,) [1..]) -> tx) ) -> hsplit (
                  box_ [onClick (NextImg Nothing)] $ image_ tt [fitWidth]
                , vstack $ P.map previewI tx 
                         <> [ box_ [onClick (NextImg (Just 6))] . label . T.pack . P.show  
                              $ (P.length (imgs m)) 
                            ]
                )
                    
        , (vscroll . vstack $ P.map (box_ [alignRight]) $ [ 
               vstack $ P.map (showMsg) (msgs m)
           ]) 
            `styleBasic` [width 330]
        
      ) `nodeVisible` (mode m == Unicorn)
    , vstack  (P.map (label . render) (keys $ pool m))
            `nodeVisible` (mode m == Bull)
    , getdoges m 
         `nodeVisible` (mode m == Doge)      
    
    ]

previewI :: (Int, Text) -> AppNode
previewI (ii, ttt) = 
    box_ [onClick (NextImg (Just ii))] (image_ ttt [fitWidth])
    `styleBasic` [width 33] 

getdoges :: AppModel -> AppNode
getdoges m = case selectedeid m of 
    Just (Selecty x ee) -> vstack $ 
        [ label $ wq x 
        , vstack (P.map showMsg ee) 
        ]  
    Nothing -> label "nothing"  

ofof :: Map Hex32 Object -> Hex32 -> AppNode
ofof mp i = case M.lookup i mp of 
    Just o -> (flip label_ labelconfig) . wq $ o
    Nothing -> label "nothing"
 
getr :: Event -> Maybe (Hex32, Object)
getr (N.Event _ _ (Content{pubkey, content})) = 
    (pubkey,) <$> byObjr content

byObjr :: Text -> Maybe Object 
byObjr (encodeUtf8 -> t) = case decode . LB.fromStrict $ t of 
    Just (Object o) -> Just o
    _ -> Nothing


    
showMsg :: Event -> AppNode
showMsg e = case kindE e of 
    Kind0 name about picture -> vstack [
          label $ name 
        , image picture
        , label about
        ]
    _ -> showMsgOG e

showMsgOG (N.Event i _ (Content{..})) = box_ [onClick (GetRe i)] $ 
    case evalState extractlinks ("", [], content)  of 
        (b4, _, _) -> label_ b5 labelconfig
            where 
            b5 = case evalState ( extractReg "#[^:space:]+" ) ("",[], b4) of 
                (b6, bx, _) -> T.intercalate ", " bx 
                    

type RegT = State (Text, [Text], Text) (Text, [Text], Text)

extractReg :: Text -> RegT  
extractReg reg  = do 
    (tb, tlx, ta) <- get  
    case ta =~ reg :: (Text, Text, Text) of 
        (t, "", "") -> pure (tb <> t, tlx, "")
        (t, ll, "") -> pure (tb <> t, ll : tlx, "")
        (blt, ll, btl) -> put (tb <> blt, ll : tlx, btl) 
                              >> extractReg reg
    
extractlinks :: RegT
extractlinks = extractReg reglinks        
    where reglinks :: Text 
          reglinks = "http.+(jpg|png)"
        
    
labelconfig :: [LabelCfg AppModel AppEvent]
labelconfig = [ O.multiline , trimSpaces]

start :: SQL.Connection -> TChan Event -> Pool -> IO ThreadId
start db ff pool@(Pool (readTVarIO -> p') _ _) = do 
    -- p' <- readTVarIO v 
    p'' <- p'
    forkOS $
      startApp 
        (mstart db p'') 
        (handle db ff pool) 
        (buildUI) 
        [ appWindowTitle "nostr"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
        , appInitEvent AppInit
        ]

