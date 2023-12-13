module Main where

import Monomer 
import Data.Text (Text, intercalate, splitOn)
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
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
import Nostr.Keys (exportPub, genKeyPair, xnpub, npub)
import Nostr.Pool (poolParty)
import Futr.Gui hiding (buildUI, handle, showImg)

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel {
        theme :: Theme,
        searchText :: Text,
        newLink :: Text,
        query :: Maybe Text
        , results :: [Event]
    } deriving (Eq)
data AppEvent = 
          AppInit
        | TextField Text
        | Fetch Filter
        | Search Text
        | LinkField Text
        | Map Text
        | Results [Event]

main :: IO ()
main = do 
        d <- (<>"/.futr") <$> getHomeDirectory
        createDirectoryIfMissing False d 
        db <- open (d <> "/events.sqlite")
        _ <- createDb db 
        kp <- dbIdentity db
        p <- poolParty db kp 
        startApp (AppModel lightTheme "" "" Nothing []) (handle db) buildUI
                [ appWindowTitle "free://space"
                , appWindowIcon "./assets/images/f_icon.png"
                , appTheme lightTheme
                , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
                , appInitEvent AppInit
                ]

handle :: Connection -> AppEnv -> AppNode -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handle db _ _ model event = case event of
        AppInit -> [ Task . pure . Fetch $ emptyF ]
        TextField t -> [ Model model {searchText = t} ]

        Fetch fi -> [ Task $ Results <$> fetch db fi ]

        Search "" -> [ Task . pure . Fetch $ emptyF ]
        Search (splitOn " " -> tx) -> [ Task $ Results <$> fetchx db 
                (map (\t -> emptyF { aztagF = [AZTag 't' t] }) tx)
                ] 

        LinkField t -> [ Model model {newLink = t} ]
        Map _ -> [ Model model {newLink = "mapping not supported yet"} ]
        Results ex -> [ Model model { results = ex } ]

myResultBox :: Text -> AppNode
myResultBox query = box_ [alignLeft] (vstack 
        [ hstack [ label ("You've already mapped [" <> query <> "] to "), externalLink "Ecosia" "https://ecosia.org" ]
        ] `styleBasic` [padding 10, radius 5, bgColor gainsboro]) `styleBasic` [padding 10]

otherResultBox :: Text -> AppNode
otherResultBox query = box_ [alignLeft] (vstack 
        [ hstack [ label ("2 friends map [" <> query <> "] to "), externalLink "DuckDuckGo" "https://duckduckgo.com/" ] `styleBasic` [ textSize 30 ]
        , externalLink "fiatjaf" "https://fiatjaf.com/" `styleBasic` [textSize 10, textLeft]
        , externalLink "zenen" "https://zenen.space" `styleBasic` [textSize 10, textLeft]

        ] `styleBasic` [textLeft, padding 10, radius 5, bgColor gainsboro]) `styleBasic` [padding 10]

addEntry :: Text -> AppModel -> AppNode
addEntry query model = box_ [alignLeft] (vstack 
        [ label ("No results for [" <> query <> "]") `styleBasic` [textSize 20, textLeft]
        , label "Would you like to add a mapping?"
        , spacer
        , keystroke [("Enter", Map $ newLink model)] $ textFieldV_ (newLink model) LinkField [placeholder "paste a URL or bech32-style nostr identifier"]

        ] `styleBasic` [textLeft, padding 10, radius 5, bgColor gainsboro]) `styleBasic` [padding 10]
    

-- results :: Text -> AppModel -> [AppNode]
-- results query model = case query of
--         "search" -> [myResultBox query, otherResultBox query]
--         _ -> [addEntry query model]

buildUI :: AppEnv -> AppModel -> AppNode
buildUI env model = vstack (
    [ vstack [
            label "free://space" `styleBasic` [textSize 40, textCenter],
            subtext
            ] `styleBasic` [padding 20]
        , tttextfield
        ]
    ++  [ hstack [ keystroke [("Enter", Search $ searchText model)] $ 
        textFieldV_ 
                (searchText model) 
                TextField 
                [placeholder "enter search tags, enter blank refreshes"]] 
        `styleBasic` [padding 20]
        ]
    ++ interface) 

    `styleBasic` [padding 20] 
        where
        subtext = case (query model) of
            Just q -> label ("Search results for [" <> q <> "]") `styleBasic` [textSize 20, textCenter]
            Nothing -> label "the world, on your terms" `styleBasic` [textSize 20, textCenter]

        interface = map showMsg2 (take 5 $ results model)
        -- case (query model) of
        --     Just q -> results q model
        --     Nothing -> []

showMsg2 :: Event -> AppNode
showMsg2 e = case kindE e of 
    Kind0 (Just (Profile name about picture addies)) -> vstack [
          hstack [
                box (label name) `styleBasic` [padding 22]
              , box (image_ picture [fitWidth]) 
                `styleBasic` [height 53, width 53]
              , label_ about lconfig 
              ]
        , vstack $ flip map addies \(t,tt) -> label (t <> " : " <> tt) 
        ]
    Kind0 Nothing -> label_ (content . con $ e) lconfig 
    Kind1 _ txt (mapMaybe isTtag -> mx) -> vstack [
          label_ txt lconfig `styleBasic` [textSize 21]
        , label_ (intercalate ", " mx) lconfig
        ]
    _ -> label "unexpected"


lconfig = [multiline, trimSpaces]

isTtag :: Tag -> Maybe Text 
isTtag (AZTag 't' x) = Just x
isTtag _ = Nothing  
