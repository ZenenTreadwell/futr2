module Main where

import Monomer as O
import Data.Text (Text)

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel {
        theme :: Theme,
        searchText :: Text,
        newLink :: Text,
	query :: Maybe Text
    } deriving (Eq)

data AppEvent = 
      AppInit
    | TextField Text
    | Search Text
    | LinkField Text
    | Map Text

main :: IO ()
main = startApp (AppModel lightTheme "" "" Nothing) handle buildUI
        [ appWindowTitle "free://space"
        , appWindowIcon "./assets/images/f_icon.png"
        , appTheme lightTheme
        , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
        , appInitEvent AppInit
        ]

handle :: AppEnv -> AppNode -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handle _ _ model event = case event of
	AppInit -> []
	TextField t -> [ Model model {searchText = t} ]
	Search "" -> [ Model model {searchText = "", query = Nothing} ]
	Search t -> [ Model model {searchText = "", query = Just t} ]
	LinkField t -> [ Model model {newLink = t} ]
	Map t -> [ Model model {newLink = "mapping not supported yet"} ]

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
    

results :: Text -> AppModel -> [AppNode]
results query model = case query of
	"search" -> [myResultBox query, otherResultBox query]
	_ -> [addEntry query model]

buildUI :: AppEnv -> AppModel -> AppNode
buildUI env model = vstack (
    [ vstack [
            label "free://space" `styleBasic` [textSize 40, textCenter],
            subtext
            ] `styleBasic` [padding 20]
        ]
    ++  [ hstack [ keystroke [("Enter", Search $ searchText model)] $ textFieldV_ (searchText model) TextField [placeholder "Who / what are you looking for? (debug note: 'search' returns demo results)"]] 
            `styleBasic` [padding 20]
        ]
    ++ interface) `styleBasic` [padding 20] where
        subtext = case (query model) of
            Just q -> label ("Search results for [" <> q <> "]") `styleBasic` [textSize 20, textCenter]
            Nothing -> label "the world, on your terms" `styleBasic` [textSize 20, textCenter]

	interface = case (query model) of
            Just q -> results q model
            Nothing -> []

