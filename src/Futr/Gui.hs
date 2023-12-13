
module Futr.Gui where 
import Futr.App
import Futr.Imgs

import Prelude as P
import Monomer as O 
import qualified Data.ByteString as B
import Data.ByteString.Lazy as LB
import Nostr.Event as N
import Nostr.Pool
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Nostr.Kinds
import Database.SQLite.Simple as SQL
import Control.Monad
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Data.Text as T
import Data.Maybe
import Text.URI
import qualified Data.Map as M 
import Network.HTTP.Req 
import Codec.Picture
import GHC.Float
import Control.Concurrent.Async
import Control.Monad.State
import Data.Time.Clock
import Data.Sequence as S
import Data.Foldable
import Data.Typeable

handle :: SQL.Connection  -> TChan Event -> Pool -> AppEnv -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handle db f (Pool p _ _) _ _ m x = case x of 
    AppInit -> [
    -- Producer fresher, i
          Producer displayfeed
                -- (\r -> do 
                -- d <- getCurrentTime
                -- evalStateT (displayfeed r) (d, [])) 
        ]
        where 
        -- fresher :: (AppEvent -> IO ()) -> IO () 
        -- fresher r = do 
        --     threadDelay 5000000
        --     readTVarIO p >>= r . FreshPool  
        --     fresher r 
        -- buffer :: NominalDiffTime
        -- buffer = secondsToNominalDiffTime 6
        
        displayfeed :: (AppEvent -> IO ()) 
                    -> IO () -- StateT (UTCTime, [Image PixelRGBA8]) IO ()
        displayfeed r = forever do 
            (getImgs . kindE -> ix) <- atomically $ readTChan f
            mapM (\uri -> fetchImg uri >>= r . LoadImg . (uri,)) ix

            
            -- let passed = diffUTCTime curr last
            -- if P.length ex > 1 && passed > buffer
            -- then do put (curr, []) 
            --         liftIO $ r . A $ e:ex
            -- else put (last, e:ex)

            
    ReModel n -> [Model n]

    Nada -> []

    LoadImg p -> [ Model m { imgs = imgs m |> p } ] -- XXX Seq 
         
        -- if M.member uri (imgl m) 
        -- then []
        -- else [ Model m {imgl = M.insert uri Nothing (imgl m)}, Task do 
        --     bs <- race (fetchImg uri) 
        --                (threadDelay 30000000) 
        --     case bs of 
        --         Right _ -> pure Nada
        --     ]
    
    TextField t -> [ Model m {texts = t} ]
    -- A e -> 
    --         let imgx = P.concatMap (getImgs . kindE) e
    --             newi = imgs m <> imgx 
    --         in [ Model $ m { 
    --                 msgs = e 
    --               , imgs = v
    --             }] <> (P.map (Task . pure . LoadImg) . P.take 5) newi 
    NextImg (fromMaybe 1 -> i) -> 
        let imp = S.drop i (imgs m)
        in [Model $ m { imgs = imp }] 
           -- <> (P.map (Task . pure . LoadImg) . S.take 5) imp 
    -- SwitchTheme t -> [Model $ m { theme = t }]
    -- FreshPool p -> 
    --     let fiv = mapMaybe fivy $ P.take 6 (imgs m)
    --         -- fivy :: URI -> Maybe _
    --         fivy u = if M.member u (imgl m)
    --                  then Nothing 
    --                  else Just (Task . pure . LoadImg $ u) 
    --     in [ Model $ m { pool = p } , Task (pure $ NextImg (Just 0)) ] 
        
getImgs :: Kind -> [URI]
getImgs (Kind1 u _ _) = u
getImgs _ = []

    
mstart :: Pool' -> AppModel
mstart p = AppModel darkTheme S.empty p "futr" 

showgg :: Text -> Maybe (Image PixelRGBA8) -> AppNode
showgg t = \case 
    Just a -> showImg t a
    _ -> label "xd"
    

-- okup :: M.Map URI -> URI -> AppNode
okup m u = showgg (render u) (M.lookup u m)


data TTTModel = TTTModel Text deriving Eq
data TTTEvent = 
      ChangeTTT Text 
    | ReportTTT

buildTTT :: UIBuilder TTTModel TTTEvent
buildTTT  _ (TTTModel t) = textFieldV t ChangeTTT

handlTTT :: (Typeable a, Typeable b) => EventHandler TTTModel TTTEvent a b  
handlTTT _ _ _ (ChangeTTT t) = [Model . TTTModel $ t]
handlTTT _ _ _ _ = []
                 -- [Report $ TextField ...]

compositeTTT 
    :: (Typeable a, Typeable b)  => TTTModel 
    -> (TTTModel -> TTTEvent) 
    -> UIBuilder TTTModel TTTEvent
    -> EventHandler TTTModel TTTEvent a b
    -> WidgetNode a b

compositeTTT = compositeV (WidgetType "ttt") 

tttextfield :: (Typeable a, Typeable b) => WidgetNode a b
tttextfield = compositeTTT (TTTModel "past") (const ReportTTT) buildTTT handlTTT



-- imgShower :: TChan Event -> ShowerModel -> (ShowerModel->ShowerEvent)
--     -> UIBuilder ShowerModel ShowerEvent
--     -> EventHandler ShowerModel ShowerEvent AppModel AppEvent  
--     -> AppNode
-- imgShower = compositeV (WidgetType "shower")

-- data ShowerModel
-- data ShowerEvent
-- showerHandle :: EventHandler TTTModel TTTEvent AppModel AppEvent  
-- showerHandle _ _ _ _ = []

-- showerBuild :: UIBuilder ShowerModel ShowerEvent
-- showerBuild _ _ = label " "

pandoras :: AppNode -> AppNode
pandoras = box_ [mergeRequired (\_ _ _ -> False)]  






-- buildUI :: AppEnv  -> AppModel -> AppNode
-- buildUI _ m = 
--     -- themeSwitch (theme m) $ -- . keystroke [("Enter", Entuh)] 
--     vstack [
--     pandoras $ compositeTTT (TTTModel "past") (const ReportTTT) buildTTT handlTTT  
--     --   textFieldV (texts m) TextField 
--     -- , vscroll . vstack . P.map showMsg $ msgs m 
--     , case imgs m of 
--             S.Empty    -> spacer
--             ((urt, tt) :<| (S.take 5 -> tx)) -> hsplit (
--                   box_ [onClick (NextImg Nothing)] $ showImg (render urt) tt
--                   -- $ showgg (render tt) (join (M.lookup tt (imgl m)))  
--                 , vstack $ 
--                     P.map 
--                         (\(ii, (ur, pi) )  -> 
--                             box_ [onClick (NextImg (Just ii))] (showImg (render ur) pi)
--                             -- (showgg "" -- (join $ M.lookup ur (imgl m)))  
--                             `styleBasic` [width 100]
--                             )
--                         (P.zipWith (,) ([1..]) (toList tx) )
                   
--                         <> 
--                         [ box_ [onClick (NextImg (Just 6))] . label . T.pack . P.show  
--                               $ S.length (imgs m) 
--                         ]
--                 )
    
--     ]
    
-- showMsg :: Event -> AppNode
-- showMsg e = case kindE e of 
--     Kind0 (Just (Profile name about picture addies)) -> vstack [
--           hstack [
--                 box (label name) `styleBasic` [padding 22]
--               , box (image_ picture [fitWidth]) 
--                 `styleBasic` [height 53, width 53]
--               , label_ about labelconfig
--               ]
--         , vstack $ flip P.map addies \(t,tt) -> label (t <> " : " <> tt) 
--         ]
--     Kind0 Nothing -> label_ (content . con $ e) labelconfig 
--     Kind1 _ txt (mapMaybe isIs -> mx) -> vstack [
--           label_ txt labelconfig `styleBasic` [textSize 21]
--         , label_ (T.intercalate ", " mx) labelconfig
--         ]
--     _ -> label "unexpected"
       
isIs :: Tag -> Maybe Text 
isIs (AZTag 't' x) = Just x
isIs _ = Nothing  
    
labelconfig :: [LabelCfg AppModel AppEvent]
labelconfig = [ O.multiline , trimSpaces]

-- start :: SQL.Connection -> TChan Event -> Pool -> IO ()
-- start db ff pool@(Pool (readTVarIO -> pio) _ _) = do 
--     p <- pio
--     startApp 
--         (mstart p) 
--         (handle db ff pool) 
--         (buildUI) 
--         [ appWindowTitle "nostr"
--         , appWindowIcon "./assets/images/icon.png"
--         , appTheme darkTheme
--         , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
--         , appInitEvent AppInit
--         ]

