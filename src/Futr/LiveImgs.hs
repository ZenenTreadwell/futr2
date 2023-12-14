module Futr.LiveImgs (liveImgs) where 

import Prelude hiding (take, drop, length)
import Monomer
import Text.URI (URI, render)
import Data.Typeable (Typeable)
import Codec.Picture (Image, PixelRGBA8)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Data.Sequence ((<|), (|>), Seq(..), take, drop, length, empty)
import Data.Foldable (toList)
import Data.Text (pack)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Nostr.Event
import Nostr.Kinds
import Futr.Imgs

data LImodel = M (Seq (URI, Image PixelRGBA8)) deriving (Eq)
data LIevent = 
      NextImg (Maybe Int)
    | LoadImg (URI, Image PixelRGBA8) 
    | PriorityLoadImg (URI, Image PixelRGBA8) 
    | WhatImg
    | InitImg

handle :: (Typeable a, Typeable b) 
             => TChan Event 
             -> EventHandler LImodel LIevent a b
handle f _ _ (M m) = \case 
    LoadImg p -> [ Model . M $ m |> p ]
    PriorityLoadImg p -> [ Model . M $ p <| m ]
    NextImg (fromMaybe 1 -> i) -> 
        let imp = drop i m
        in [Model . M $ imp] 
    WhatImg -> []
    InitImg -> 
        let feed r = forever do 
                (getImgs . kindE -> ix) <- atomically $ readTChan f
                mapM (\uri -> fetchImg uri >>= r . LoadImg . (uri,)) ix
            getImgs :: Kind -> [URI]
            getImgs (Kind1 u _ _) = u
            getImgs _ = []
        in [Producer feed] 

build :: UIBuilder LImodel LIevent
build _ (M ix) = case ix of 
            Empty    -> spacer
            ((urt, tt) :<| (take 5 -> tx)) -> hsplit (
                  box_ [onClick (NextImg Nothing)] $ showImg (render urt) tt
                  -- $ showgg (render tt) (join (M.lookup tt (imgl m)))  
                , vstack $ 
                    map 
                        (\(ii, (ur, pi) )  -> 
                            box_ [onClick (NextImg (Just ii))] (showImg (render ur) pi)
                            -- (showgg "" -- (join $ M.lookup ur (imgl m)))  
                            `styleBasic` [width 100]
                            )
                        (zipWith (,) ([1..]) (toList tx) )
                   
                        <> 
                        [ box_ [onClick (NextImg (Just 6))] . label . pack . show  
                              $ length ix 
                        ]
                    )
    
liveImgs :: (Typeable a, Typeable b) => TChan Event -> WidgetNode a b
liveImgs f = compositeV_ 
    (WidgetType "sh")
    (M empty) 
    (const WhatImg)
    build
    (handle f)
    [onInit InitImg]
