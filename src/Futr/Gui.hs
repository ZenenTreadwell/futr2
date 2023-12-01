
module Futr.Gui where 

import Prelude as P
import Monomer as O 

import qualified Data.ByteString as B
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
import Data.Text as T
import Data.Text.Encoding as T
import Data.Maybe
import Text.URI
import Text.Regex.TDFA
import Monomer.Widgets.Singles.Base.InputField
import Data.Aeson as J
import qualified Data.Map as M 
import Data.Map (Map, keys)
import Network.HTTP.Req 
import Data.Proxy
import Codec.Picture
import GHC.Float

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel {
        theme :: Theme 
      , msgs :: [Event] -- Graph
      , imgs :: [URI]
      , imgl :: M.Map URI (Image PixelRGBA8) 
      , pool :: Pool'
      , texts :: Text
      , selectedeid :: Maybe Selecty
    } deriving (Eq)

data Selecty = Selecty Hex32 [Event] deriving (Eq, Show)

data AppEvent = 
      AppInit
    | ReModel AppModel
    | TextField Text
    | FreshPool Pool'
    | SwitchTheme Theme
    | A Event
    | GetRe Hex32
    | NextImg (Maybe Int)
    | LoadImg URI 

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

    LoadImg uri -> 
        if M.member uri (imgl m) 
        then []
        else [ Task do 
            bs <- fetchImg uri 
            pure $ ReModel m { imgl = M.insert uri bs (imgl m) } 
            ]
    
    TextField t -> [ Model m {texts = t} ]
    A e -> case kind . con $ e of 
        0 -> [Model m { msgs = e : msgs m }]
        1 -> P.map (Task . pure . LoadImg) (imgs m) 
            <>
            [ Task $ do 
            let (Kind1 imgx _ _) = kind1 e
            pure . ReModel $ m { 
                  msgs = e : (P.take 5 $ msgs m)
                , imgs = (imgs m) <> imgx
                }]
        _ -> []
    NextImg (fromMaybe 1 -> i) -> 
        let imp = P.drop i (imgs m)
            fiv = P.map (Task . pure . LoadImg) $ P.take 5 imp  
        in fiv <> [
        Model $ m { imgs = imp }]
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
        
fetchImg :: URI -> IO (Image PixelRGBA8) -- ByteString
fetchImg uri = 
    let ocd urri = req GET urri NoReqBody bsResponse mempty 
        ree :: HttpResponseBody BsResponse -> B.ByteString
        ree = id
        -- imgee :: B.ByteString -> 
    in do 
    (ree . responseBody -> bs) <- runReq defaultHttpConfig $  
        case useURI uri of 
            Just (Right (fst -> r)) -> ocd r
            Just (Left (fst -> l)) -> ocd l -- error "what"
            Nothing -> error "bad uri?"
    
    -- pure 
    pure case decodeImage bs of 
        Left l -> error l 
        Right r -> convertRGBA8 r 
        -- case encodeDynamicBitmap $ ImageRGBA8 (convertRGBA8 r) of 
        --     Left l -> error l
        --     Right reee -> reee

showImg :: Text -> Image PixelRGBA8 -> AppNode 
showImg label rgba = imageMem_ label 
                               (toStrict $ encodeBitmap rgba) 
                               (Size (int2Double $ imageWidth rgba) 
                                     (int2Double $ imageHeight rgba))  --[fitWidth] 
                               [fitWidth]
        
    
mstart :: SQL.Connection -> Pool' -> AppModel
mstart o p = AppModel darkTheme [] [] M.empty p "futr" Nothing


-- lookupImg :: M.Map URI (Image PixelRGBA8) -> URI -> Maybe (Image PixelRGBA8) 
-- lookupImg loa uri = M.lookup uri loa

-- lookImg 

showgg t = \case 
    Just a -> showImg t a
    _ -> label "xd"
    

-- okup :: M.Map URI -> URI -> AppNode
okup m u = showgg (render u) (M.lookup u m)

buildUI :: AppEnv  -> AppModel -> AppNode
buildUI _ m = 
    themeSwitch (theme m) -- . keystroke [("Enter", Entuh)] 
    $ vstack [
      textFieldV (texts m) TextField 
    , case imgs m of 
            []    -> spacer
            (tt : tx) -> hsplit (
                  box_ [onClick (NextImg Nothing)] $ showgg (render tt) (M.lookup tt (imgl m))  
                , vstack $ 
                    P.map 
                        (\(ii, ur) -> 
                            box_ [onClick (NextImg (Just (ii + 1) ))] (showgg (render ur) (M.lookup ur (imgl m)))  
                            `styleBasic` [width 100]
                            )
                        (P.zipWith (,) [0..] tx) 
                    
                   
                        <> 
                        [ box_ [onClick (NextImg (Just 6))] . label . T.pack . P.show  
                              $ (P.length (imgs m)) 
                        ]
                )
    
    ]
    
showMsg :: Event -> AppNode
showMsg e = case kindE e of 
    Kind0 (Just (Profile name about picture addies)) -> vstack [
          hstack [
                box (label name) `styleBasic` [padding 22]
              , box (image_ picture [fitWidth]) 
                `styleBasic` [height 53, width 53]
              , label_ about labelconfig
              ]
        , vstack $ flip P.map addies \(t,tt) -> label (t <> " : " <> tt) 
        ]
    Kind0 Nothing -> label (content . con $ e)
    Kind1 _ txt (mapMaybe isIs -> mx) -> hstack [
          label_ txt labelconfig `styleBasic` [textSize 21]
        -- , hstack (P.map previewJ memex)
        , label (T.intercalate ", " mx)
        ]
    _ -> label "unexpected"
       
isIs :: Tag -> Maybe Text 
isIs (AZTag 't' x) = Just x
isIs _ = Nothing  

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
 
    
labelconfig :: [LabelCfg AppModel AppEvent]
labelconfig = [ O.multiline , trimSpaces]

start :: SQL.Connection -> TChan Event -> Pool -> IO ()
start db ff pool@(Pool (readTVarIO -> p') _ _) = do 
    -- p' <- readTVarIO v 
    p'' <- p'
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

