module Main where

import Monomer hiding (Event)
import Data.Text (Text, intercalate, splitOn, pack)
import Text.URI (render)
import Data.Maybe (mapMaybe)
import Control.Concurrent.STM.TChan
import System.Directory (
          getHomeDirectory
        , createDirectoryIfMissing
        )
import Database.SQLite.Simple (
          open
        , Connection ()
        )
import Nostr.Event 
import Nostr.Kinds
import Nostr.Keys 
import Nostr.Beam
import Nostr.Db

import Futr.Gui 
import Futr.LiveImgs 
import Futr.Imgs
import Futr.TagSearch
import Futr.App
import Futr.Pool (poolParty, extractURI)

import Data.Typeable


main :: IO ()
main = do 
    d <- (<>"/.futr") <$> getHomeDirectory
    createDirectoryIfMissing False d 
    db <- open (d <> "/events.sqlite")
    f <- createDb db 
    kp <- dbIdentity db
    pub <- exportPub kp
    p <- poolParty db kp 
    t <- newTChanIO
    let futr = Futr p f db t
    _ <- fetchHex futr pub
    e <- signE kp Content 
    startApp (AppModel e [] Uni) (appHandle futr) (appBuild futr)
            [ appWindowTitle "futr"
            , appWindowIcon "./assets/images/icon.png"
            , appTheme lightTheme
            , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
            , appInitEvent AppInit
            ]

appBuild :: Futr -> AppUIBuilder AppModel AppEvent 
appBuild futr _ m@(AppModel{showMode}) = flip styleBasic [padding 20] . vstack $ [ 
        box drawImg `styleBasic` [height 250]
      , hstack [ 
            -- button "doge"  (SetPage Doge)

          -- , 
            button "explore?" (SetPage Uni) `styleBasic` [width 123]     
          -- , button "bull" (SetPage Bull)
          , button "imgfeed" (SetPage ImgFeed) `styleBasic` [width 123]     
          ]
    , zstack $ [
          -- label "accounts" `nodeVisible` (showMode == Doge)
        -- , 
          nostr futr m `nodeVisible` (showMode == Uni)
        -- , label "relays" `nodeVisible` (showMode == Bull)
        , liveImgs futr `nodeVisible` (showMode == ImgFeed)
    ]]

nostr :: Futr -> AppModel -> AppNode
nostr futr (AppModel e ex _) = 
    vscroll . vstack $ [ 
          currentCard futr e `styleBasic` [textSize 20, textCenter, padding 15]
        , vstack (map (showMsg2 futr ) ex) `styleBasic` [textSize 17, textLeft]
        , tagSearch futr 
    ]

currentCard :: Futr -> Event -> AppNode
currentCard futr e@(Event _ _ (Content {..})) = case kindE e of 
    Kind0 (Just (Profile name about picture banner addies)) -> vstack [
              box (image_ picture [fitWidth, alignCenter]) 
                  `styleBasic` [width 60, height 60, radius 30]
            , label name `styleBasic` [textSize 33, textCenter]
            , label "subtext" `styleBasic` [textSize 20, textCenter]
            , directMessage futr
            ] `styleBasic` [padding 20]

    Kind1 _ ol txt xtg -> showMsg2 futr e
    _ -> label $ "unexpected kind" <> (pack . show $ kind)

directMessage _ = tttextfield 
        

showMsg2 :: (Typeable a, Typeable b) => Futr -> Event -> WidgetNode a b
showMsg2 futr e@(Event _ _ (Content {..})) = case kindE e of 
    Kind0 (Just (Profile name about picture banner addies)) -> vstack [
          hstack [
                box (label name) `styleBasic` [padding 22]
              , box (image_ picture [fitWidth]) 
                `styleBasic` [height 53, width 53]
              , label_ about lconfig 
              ]
        , vstack $ flip map addies \(t,tt) -> label (t <> " : " <> tt) 
        ]
    Kind0 Nothing -> label "0 but no profile" -- _ content lconfig 
    Kind1 _ ol txt (mapMaybe isTtag . (<> tags) -> mx) -> vstack [
          label_ txt lconfig 
                `styleBasic` [textSize 21]
        -- , separatorLine
        , label_ (intercalate ", " mx) lconfig 
                `styleBasic` [textSize 12]
        , vstack $ mapMaybe (\o -> do
                (a, _, _) <- extractURI o 
                Just $ externalLink (pack a) (render o)) ol 
        -- , separatorLine
        ]
    _ -> label "not 1 or 0"


isTtag :: Tag -> Maybe Text 
isTtag (AZTag 't' x) = Just x
isTtag _ = Nothing  
