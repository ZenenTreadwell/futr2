module Main where

import Monomer as O

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel {
        theme :: Theme 
    } deriving (Eq)

data AppEvent = 
      AppInit

main :: IO ()
main = startApp (AppModel lightTheme) handle buildUI
        [ appWindowTitle "nostr"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Cantarell-Regular.ttf"
        , appInitEvent AppInit
        ]

handle :: AppEnv -> AppNode -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handle _ _ model event = case event of
	AppInit -> []

buildUI :: AppEnv -> AppModel -> AppNode
buildUI env model = vstack [label "free://space", label "nice"]
