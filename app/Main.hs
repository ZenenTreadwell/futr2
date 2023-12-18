module Main where

import Monomer 
import Data.Text (Text, intercalate, splitOn)
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
        -- (exportPub, genKeyPair, xnpub, npub)
import Nostr.Pool (poolParty)
import Nostr.Filter 
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
    _ <- fetchHex futr pub
    sec :: Integer <- round <$> getPOSIXTime
    e <- signE kp $ Content 1 [] "this is a test"  sec
    startApp (AppModel e [] Nothing) (appHandle futr) (appBuild futr)
            [ appWindowTitle "futr"
            , appWindowIcon "./assets/images/icon.png"
            , appTheme lightTheme
            , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
            , appInitEvent AppInit
            ]

appHandle :: Futr -> AppEventHandler AppModel AppEvent
appHandle futr@(Futr{base, top}) _ _ model event = case event of
    AppInit -> [Producer (atomically (readTChan top) >>=)]
    SetCurrent x -> [Task $ ApplyCurrent <$> fetchHex futr x ]
    ApplyCurrent (Just (e, ex)) -> [Model $ AppModel e ex Nothing]
    _ -> []

appBuild :: Futr -> AppUIBuilder AppModel AppEvent 
appBuild futr _ _ = vstack [
    vstack [
            label "construction" `styleBasic` [textSize 40, textCenter],
            label "subtext" `styleBasic` [textSize 20, textCenter]
            
            ] `styleBasic` [padding 20]
    , hsplit (tagSearch futr, liveImgs futr)
    ]
    `styleBasic` [padding 20] 

fetchHex :: Futr -> Hex32 -> IO (Maybe (Event, [Event])) 
fetchHex (Futr{base}) x = do 
    frompub <- fetch base emptyF{
                              authorsF=Just (Authors [wq x]) 
                            , kindsF=Just (Kinds [0])
                            } 
    case frompub of 
        [] -> pure Nothing
        e : _ -> pure . Just $ (e, [])
