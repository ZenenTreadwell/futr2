module Main where

import Monomer hiding (Event)
import Data.Text (Text, intercalate, splitOn, pack)
import Text.URI (render)
import Data.Maybe (mapMaybe)
import Data.Time.Clock.POSIX
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import System.Directory (
          getHomeDirectory
        , createDirectoryIfMissing
        )
import Database.SQLite.Simple (
          open
        , Connection ()
        )
import Nostr.Beam (dbIdentity, createDb, fetch, fetchx)

import Nostr.Filter (
          emptyF 
        , Filter (..)
        , Kinds(Kinds)
        )
import Nostr.Event 
import Nostr.Kinds
import Nostr.Keys 
import Nostr.Pool
        -- (exportPub, genKeyPair, xnpub, npub)
import Nostr.Pool (poolParty)
import Nostr.Filter 
import Futr.Gui 
import Futr.LiveImgs 
import Futr.TagSearch
import Futr.App

import Data.Typeable
import Control.Concurrent

main :: IO ()
main = do 
    d <- (<>"/.futr") <$> getHomeDirectory
    createDirectoryIfMissing False d 
    db <- open (d <> "/events.sqlite")
    f <- createDb db 
    kp <- dbIdentity db
    pub <- exportPub kp
    p <- poolParty db kp 
    -- threadDelay 60000000
    t <- newTChanIO
    let futr = Futr p f db t
    _ <- fetchHex futr pub
    sec :: Integer <- round <$> getPOSIXTime
    e <- signE kp $ Content 1 [] "this is a test"  sec
    startApp (AppModel e [] Uni) (appHandle futr) (appBuild futr)
            [ appWindowTitle "futr"
            , appWindowIcon "./assets/images/icon.png"
            , appTheme lightTheme
            , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
            , appInitEvent AppInit
            ]

appBuild :: Futr -> AppUIBuilder AppModel AppEvent 
appBuild futr _ m@(AppModel{showMode}) = flip styleBasic [padding 20] . vstack $ [ 
      hstack [ 
            button "doge"  (SetPage Doge)
          , button "unicorn" (SetPage Uni) `styleBasic` [width 123]     
          , button "bull" (SetPage Bull)
          , button "uncensored" (SetPage ImgFeed) `styleBasic` [width 123]     
          ]
    , zstack $ [
          label "accounts" `nodeVisible` (showMode == Doge)
        , nostr futr m `nodeVisible` (showMode == Uni)
        , label "relays" `nodeVisible` (showMode == Bull)
        , liveImgs futr `nodeVisible` (showMode == ImgFeed)
    ]]

nostr futr (AppModel e ex _) = 
    vscroll . vstack $ [ 
          showMsg2 futr e `styleBasic` [textSize 27, textCenter, padding 25]
        , vstack (map (showMsg2 futr) ex) `styleBasic` [textSize 17, textLeft]
        , tagSearch futr 
    ]
        
    --         label "construction" `styleBasic` 
    --         label "subtext" `styleBasic` [textSize 20, textCenter]
            
    --         ] `styleBasic` [padding 20]

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
    Kind0 Nothing -> label_ content lconfig 
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
    _ -> label "unexpected"


isTtag :: Tag -> Maybe Text 
isTtag (AZTag 't' x) = Just x
isTtag _ = Nothing  
