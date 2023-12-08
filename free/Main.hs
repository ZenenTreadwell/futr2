module Main where

import Monomer as O
import Data.Text (Text)

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel {
        theme :: Theme,
        searchText :: Text,
	query :: Maybe Text
    } deriving (Eq)

data AppEvent = 
      AppInit
    | TextField Text
    | Search Text

main :: IO ()
main = startApp (AppModel lightTheme "" Nothing) handle buildUI
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
	Search t -> [ Model model {searchText = "", query = Just t} ]

results :: Text -> [AppNode]
results query = [label query, label query, label query]

buildUI :: AppEnv -> AppModel -> AppNode
buildUI env model = keystroke [("Enter", Search $ searchText model)] $ vstack $
    [ vstack [
            label "free://space" `styleBasic` [textSize 40, textCenter],
            subtext
            ] `styleBasic` [padding 20]
        ]
    ++  [ hstack [ textFieldV_ (searchText model) TextField [placeholder "Who / what are you looking for?"]] 
            `styleBasic` [ padding 20]
        ]
    ++ interface where
        subtext = case query model of
            Just q -> label ("Search results for [" <> q <> "]") `styleBasic` [textSize 20, textCenter]
            Nothing -> label "the world, on your terms" `styleBasic` [textSize 20, textCenter]

	interface = case query model of
            Just q -> results q
            Nothing -> []

