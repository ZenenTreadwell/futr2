module Futr.TagSearch (tagSearch) where 

import Monomer hiding (Event)
import Data.Text (Text, intercalate, pack, splitOn)
import Data.Typeable
import Data.Maybe
import Database.SQLite.Simple

import Futr.App
import Futr.Gui

import Nostr.Event
import Nostr.Filter
import Nostr.Beam
import Nostr.Kinds
import Nostr.Keys
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

data TSmodel = TS Text [Event] deriving Eq

data TSevent = 
          TSInit 
        | Fetch Filter
        | Search Text
        | Results [Event]
        | TextField Text
        | SetCurrentTop Hex32
        | TSnull


handle (Futr{base, top}) _ _ (TS sm em) event = case event of
    TSInit -> [ Task . pure . Fetch $ emptyF ]
    Fetch fi -> [ Task $ Results <$> fetch base fi ]
    Search "" -> [ Task . pure . Fetch $ emptyF ]
    Search (splitOn " " -> tx) -> [ Task $ Results <$> fetchx base 
            (map (\t -> emptyF { aztagF = [AZTag 't' t] }) tx)
            ] 
    Results ex -> [ Model $ TS "" ex ]
    --- XXX
    TextField t -> [ Model $ TS t em]
    SetCurrentTop x -> [Task $ do 
        atomically (writeTChan top (SetCurrent x))
        pure TSnull 
        ]
    TSnull -> []

build _ (TS sm results) = vstack [
      keystroke [("Enter", Search sm)] $ 
          textFieldV_ 
                  sm 
                  TextField 
                  [placeholder 
                  "enter search tags, enter refreshes"] 
    , vstack $ map s2 (take 5 results)
    ]

s2 e = hsplit (
      profilePic (pubkey . con $ e)
    , box_ 
            [onClick (SetCurrentTop (eid e))] 
            (label_ (content . con $ e) lconfig) 
    )


profilePic x = button "xo" (SetCurrentTop x)




tagSearch :: (Typeable a, Typeable b) => Futr -> WidgetNode a b
tagSearch futr = pandoras $ compositeV_ 
    (WidgetType "ts")
    (TS "" [])
    (const TSnull) -- XXX?
    build
    (handle futr)
    [onInit TSInit]
    
-- -- handle :: Connection -> AppEnv -> AppNode -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]



-- tagSearch = undefined 

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
