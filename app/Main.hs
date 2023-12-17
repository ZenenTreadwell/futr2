module Main where

import Monomer 
import Data.Text (Text, intercalate, splitOn)
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

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel 
        -- {
        -- theme :: Theme,
        

        
        -- newLink :: Text,
        -- query :: Maybe Text
          -- searchText :: Text
        -- , current :: Either Event Profile 
        -- , results :: [Event]
        
    deriving (Eq)
data AppEvent = 
          AppInit
        | TextField Text
        | Fetch Filter
        | Search Text
        | Results [Event]
        | SetCurrent Hex32
        | SetCurrentE Event
        | SetCurrentP Profile  -- pubkey / eid
        -- | LinkField Text
        -- | Map Text

main :: IO ()
main = do 
        d <- (<>"/.futr") <$> getHomeDirectory
        createDirectoryIfMissing False d 
        db <- open (d <> "/events.sqlite")
        f <- createDb db 
        kp <- dbIdentity db
        pub <- exportPub kp
        p <- poolParty db kp 
        startApp (AppModel) (handle db) (buildUI db f)
                [ appWindowTitle "nostr"
                , appWindowIcon "./assets/images/icon.png"
                , appTheme lightTheme
                , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
                , appInitEvent AppInit
                ]

handle :: Connection -> AppEnv -> AppNode -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handle db _ _ model event = case event of
        _ -> []
        -- AppInit -> [ Task . pure . Fetch $ emptyF ]
        -- SetCurrentE h32 -> [ Model model { current = Left h32 } ]
        -- SetCurrentP h32 -> [ Model model { current = Right h32 } ]
        -- SetCurrent h32 -> [Task $ getH32 db h32]

getH32 :: Connection -> Hex32 -> IO AppEvent 
getH32 db h32 = undefined  

        -- LinkField t -> [ Model model {newLink = t} ]
        -- Map _ -> [ Model model {newLink = "mapping not supported yet"} ]

buildUI :: Connection -> TChan Event -> AppEnv -> AppModel -> AppNode
buildUI db f env model = vstack [
    vstack [
            label "construction" `styleBasic` [textSize 40, textCenter],
            label "subtext" `styleBasic` [textSize 20, textCenter]
            
            ] `styleBasic` [padding 20]
    , hsplit (tagSearch db, liveImgs f )
    ]
    `styleBasic` [padding 20] 

-- myResultBox :: Text -> AppNode
-- myResultBox query = box_ [alignLeft] (vstack 
--         [ hstack [ label ("You've already mapped [" <> query <> "] to "), externalLink "Ecosia" "https://ecosia.org" ]
--         ] `styleBasic` [padding 10, radius 5, bgColor gainsboro]) `styleBasic` [padding 10]

-- otherResultBox :: Text -> AppNode
-- otherResultBox query = box_ [alignLeft] (vstack 
--         [ hstack [ label ("2 friends map [" <> query <> "] to "), externalLink "DuckDuckGo" "https://duckduckgo.com/" ] `styleBasic` [ textSize 30 ]
--         , externalLink "fiatjaf" "https://fiatjaf.com/" `styleBasic` [textSize 10, textLeft]
--         , externalLink "zenen" "https://zenen.space" `styleBasic` [textSize 10, textLeft]

--         ] `styleBasic` [textLeft, padding 10, radius 5, bgColor gainsboro]) `styleBasic` [padding 10]

-- addEntry :: Text -> AppModel -> AppNode
-- addEntry query model = box_ [alignLeft] (vstack 
--         [ label ("No results for [" <> query <> "]") `styleBasic` [textSize 20, textLeft]
--         , label "Would you like to add a mapping?"
--         , spacer
--         , keystroke [("Enter", Map $ newLink model)] $ textFieldV_ (newLink model) LinkField [placeholder "paste a URL or bech32-style nostr identifier"]

--         ] `styleBasic` [textLeft, padding 10, radius 5, bgColor gainsboro]) `styleBasic` [padding 10]
    

-- results :: Text -> AppModel -> [AppNode]
-- results query model = case query of
--         "search" -> [myResultBox query, otherResultBox query]
--         _ -> [addEntry query model]
