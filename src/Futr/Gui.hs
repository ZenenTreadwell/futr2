
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
import Control.Concurrent.Async

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel {
        theme :: Theme 
      , msgs :: [Event] -- Graph
      , imgs :: [URI]
      , imgl :: M.Map URI (Image PixelRGBA8) 
      , pool :: Pool'
      , texts :: Text
    } deriving (Eq)

data AppEvent = 
      AppInit
    | Nada
    | ReModel AppModel
    | TextField Text
    | FreshPool Pool'
    | SwitchTheme Theme
    | A Event
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

    Nada -> []

    LoadImg uri -> 
        if M.member uri (imgl m) 
        then [Model m]
        else [ Task do 
            bs <- race (fetchImg uri) 
                       (threadDelay 30000000) 
            case bs of 
                Left b -> pure $ ReModel m { imgl = M.insert uri b (imgl m) } 
                Right _ -> pure Nada
            ]
    
    TextField t -> [ Model m {texts = t} ]
    A e -> case kind . con $ e of 
        1 ->  
            let (Kind1 imgx _ _) = kind1 e
                newi = imgs m <> imgx 
            in [ Model $ m { 
                  msgs = e : (P.take 5 $ msgs m)
                , imgs = newi
                }] 
        _ -> []
    NextImg (fromMaybe 1 -> i) -> 
        let imp = P.drop i (imgs m)
            fiv = P.map (Task . pure . LoadImg) $ P.take 5 imp  
        in [Model $ m { imgs = imp }] <> fiv
    SwitchTheme t -> [Model $ m { theme = t }]
    FreshPool p -> 
        let fiv = mapMaybe fivy $ P.take 6 (imgs m)
            -- fivy :: URI -> Maybe _
            fivy u = if M.member u (imgl m)
                     then Nothing 
                     else Just (Task . pure . LoadImg $ u) 
        in [ Model $ m { pool = p } , Task (pure $ NextImg (Just 0)) ] 
        
fetchImg :: URI -> IO (Image PixelRGBA8) -- ByteString
fetchImg uri = 
    let ocd u = req GET u NoReqBody bsResponse mempty 
        ree :: HttpResponseBody BsResponse -> B.ByteString
        ree = id
        -- imgee :: B.ByteString -> 
    in do 
    (ree . responseBody -> bs) <- runReq defaultHttpConfig $  
        case useURI uri of 
            Just (Right (fst -> r)) -> ocd r
            Just (Left (fst -> l)) -> ocd l -- error "what"
            Nothing -> error "bad uri?"
            
    pure case decodeImage bs of 
        Left l -> error l 
        Right r -> convertRGBA8 r 

showImg :: Text -> Image PixelRGBA8 -> AppNode 
showImg l r = imageMem_ l 
                        (toStrict $ encodeBitmap r) 
                        (Size (int2Double $ imageWidth r) 
                              (int2Double $ imageHeight r))  --[fitWidth] 
                        [fitWidth]
        
    
mstart :: Pool' -> AppModel
mstart p = AppModel darkTheme [] [] M.empty p "futr" 

showgg :: Text -> Maybe (Image PixelRGBA8) -> AppNode
showgg t = \case 
    Just a -> showImg t a
    _ -> label "xd"
    

-- okup :: M.Map URI -> URI -> AppNode
okup m u = showgg (render u) (M.lookup u m)

buildUI :: AppEnv  -> AppModel -> AppNode
buildUI _ m = 
    -- themeSwitch (theme m) $ -- . keystroke [("Enter", Entuh)] 
    vstack [
      textFieldV (texts m) TextField 
    -- , vstack . P.map showMsg $ msgs m 
    , case imgs m of 
            []    -> spacer
            (tt : (P.take 5 -> tx) ) -> hsplit (
                  box_ [onClick (NextImg Nothing)] $ showgg (render tt) (M.lookup tt (imgl m))  
                , vstack $ 
                    P.map 
                        (\(ii, ur) -> 
                            box_ [onClick (NextImg (Just ii))] (showgg (render ur) (M.lookup ur (imgl m)))  
                            `styleBasic` [width 100]
                            )
                        (P.zipWith (,) [1..] tx) 
                   
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
    Kind0 Nothing -> label_ (content . con $ e) labelconfig 
    Kind1 _ txt (mapMaybe isIs -> mx) -> vstack [
          label_ txt labelconfig `styleBasic` [textSize 21]
        , label_ (T.intercalate ", " mx) labelconfig
        ]
    _ -> label "unexpected"
       
isIs :: Tag -> Maybe Text 
isIs (AZTag 't' x) = Just x
isIs _ = Nothing  
    
labelconfig :: [LabelCfg AppModel AppEvent]
labelconfig = [ O.multiline , trimSpaces]

start :: SQL.Connection -> TChan Event -> Pool -> IO ()
start db ff pool@(Pool (readTVarIO -> pio) _ _) = do 
    p <- pio
    startApp 
        (mstart p) 
        (handle db ff pool) 
        (buildUI) 
        [ appWindowTitle "nostr"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
        , appInitEvent AppInit
        ]

