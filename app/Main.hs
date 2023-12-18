module Main where

import Monomer 
import Data.Text (Text, intercalate, splitOn)
import Text.URI (render)
import Data.Maybe (mapMaybe)
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
        -- (exportPub, genKeyPair, xnpub, npub)
import Nostr.Pool (poolParty)
import Futr.Gui 
import Futr.LiveImgs 
import Futr.TagSearch
import Futr.App

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
    startApp (AppModel) (appHandle futr) (appBuild futr)
            [ appWindowTitle "futr"
            , appWindowIcon "./assets/images/icon.png"
            , appTheme lightTheme
            , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
            , appInitEvent AppInit
            ]

appHandle :: Futr -> AppEventHandler AppModel AppEvent
appHandle (Futr{top}) _ _ model event = case event of
    AppInit -> 
        let p r = atomically (readTChan top) >>= r  
        in [Producer p]
    SetCurrent x -> []

appBuild :: Futr -> AppUIBuilder AppModel AppEvent 
appBuild futr env model = vstack [
    vstack [
            label "construction" `styleBasic` [textSize 40, textCenter],
            label "subtext" `styleBasic` [textSize 20, textCenter]
            
            ] `styleBasic` [padding 20]
    , hsplit (tagSearch futr, liveImgs futr)
    ]
    `styleBasic` [padding 20] 
