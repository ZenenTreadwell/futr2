
module Futr.Gui where 
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
        
getImgs :: Kind -> [URI]
getImgs (Kind1 u _ _) = u
getImgs _ = []

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

data ShowerModel = SM (Seq (URI, Image PixelRGBA8)) deriving (Eq)
data ShowerEvent = 
      NextImg (Maybe Int)
    | LoadImg (URI, Image PixelRGBA8) 
    | WhatImg
    | InitImg

showerHandle :: (Typeable a, Typeable b) => TChan Event -> EventHandler ShowerModel ShowerEvent a b
showerHandle f _ _ (SM m) = \case 
    LoadImg p -> [ Model . SM $ m |> p ]
    NextImg (fromMaybe 1 -> i) -> 
        let imp = S.drop i m
        in [Model . SM $ imp] 
    WhatImg -> []
    InitImg -> 
        let feed r = forever do 
                (getImgs . kindE -> ix) <- atomically $ readTChan f
                mapM (\uri -> fetchImg uri >>= r . LoadImg . (uri,)) ix
                    
        in [Producer feed] 

showerBuild :: UIBuilder ShowerModel ShowerEvent
showerBuild _ (SM ix) = case ix of 
            S.Empty    -> spacer
            ((urt, tt) :<| (S.take 5 -> tx)) -> hsplit (
                  box_ [onClick (NextImg Nothing)] $ showImg (render urt) tt
                  -- $ showgg (render tt) (join (M.lookup tt (imgl m)))  
                , vstack $ 
                    P.map 
                        (\(ii, (ur, pi) )  -> 
                            box_ [onClick (NextImg (Just ii))] (showImg (render ur) pi)
                            -- (showgg "" -- (join $ M.lookup ur (imgl m)))  
                            `styleBasic` [width 100]
                            )
                        (P.zipWith (,) ([1..]) (toList tx) )
                   
                        <> 
                        [ box_ [onClick (NextImg (Just 6))] . label . T.pack . P.show  
                              $ S.length ix 
                        ]
                    )
    
memeShower :: (Typeable a, Typeable b) => TChan Event -> WidgetNode a b
memeShower f = pandoras $ compositeV_ 
    (WidgetType "sh")
    (SM S.empty) 
    (const WhatImg)
    showerBuild
    (showerHandle f)
    [onInit InitImg]


pandoras :: 
    (Typeable a, Typeable b) => 
    WidgetNode a b -> WidgetNode a b
pandoras = box_ [mergeRequired (\_ _ _ -> False)]  


     
isIs :: Tag -> Maybe Text 
isIs (AZTag 't' x) = Just x
isIs _ = Nothing  
    
labelconfig :: (Typeable a, Typeable b) =>  [LabelCfg a b]
labelconfig = [O.multiline, trimSpaces]

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

