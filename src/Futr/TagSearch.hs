module Futr.TagSearch (tagSearch) where 

import Monomer hiding (Event)
import Data.Text (Text, intercalate, pack, splitOn)
import Data.Typeable
import Data.Maybe
import Database.SQLite.Simple
import Futr.Gui

import Nostr.Event
import Nostr.Filter
import Nostr.Beam
import Nostr.Kinds

data TSmodel = TS Text [Event] deriving Eq

data TSevent = 
          TSInit 
        | Fetch Filter
        | Search Text
        | Results [Event]
        | TextField Text
        | TSnull


handle db _ _ (TS sm em) event = case event of
    TSInit -> [ Task . pure . Fetch $ emptyF ]
    Fetch fi -> [ Task $ Results <$> fetch db fi ]
    Search "" -> [ Task . pure . Fetch $ emptyF ]
    Search (splitOn " " -> tx) -> [ Task $ Results <$> fetchx db 
            (map (\t -> emptyF { aztagF = [AZTag 't' t] }) tx)
            ] 
    Results ex -> [ Model $ TS "" ex ]
    --- XXX
    TextField t -> [ Model $ TS t em]
    TSnull -> []


build _ (TS sm results) = vstack [
      keystroke [("Enter", Search sm)] $ 
          textFieldV_ 
                  sm 
                  TextField 
                  [placeholder 
                  "enter search tags, enter refreshes"] 
    , vstack $ map showMsg2 (take 5 results)
    ]

showMsg2 e = label_ (content . con $ e) lconfig 

tagSearch :: (Typeable a, Typeable b) => Connection -> WidgetNode a b
tagSearch db = pandoras $ compositeV_ 
    (WidgetType "ts")
    (TS "" [])
    (const TSnull) -- XXX?
    build
    (handle db)
    [onInit TSInit]
-- -- handle :: Connection -> AppEnv -> AppNode -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]



-- tagSearch = undefined 


-- showMsg2 :: (Typeable a, Typeable b) => Event -> WidgetNode a b
-- showMsg2 e@(Event _ _ (Content {..})) = case kindE e of 
--     Kind0 (Just (Profile name about picture banner addies)) -> vstack [
--           hstack [
--                 box (label name) `styleBasic` [padding 22]
--               , box (image_ picture [fitWidth]) 
--                 `styleBasic` [height 53, width 53]
--               , label_ about lconfig 
--               ]
--         , vstack $ flip map addies \(t,tt) -> label (t <> " : " <> tt) 
--         ]
--     Kind0 Nothing -> label_ content lconfig 
--     Kind1 _ ol txt (mapMaybe isTtag . (<> tags) -> mx) -> vstack [
--           label_ txt lconfig 
--                 `styleBasic` [textSize 21]
--         -- , separatorLine
--         , label_ (intercalate ", " mx) lconfig 
--                 `styleBasic` [textSize 12]
--         , vstack $ mapMaybe (\o -> 
--                 let Just (a, _, _) = extractURI o 
--                 in Just $ externalLink (pack a) (render o)) ol 
--         -- , separatorLine
--         ]
--     _ -> label "unexpected"

-- isTtag :: Tag -> Maybe Text 
-- isTtag (AZTag 't' x) = Just x
-- isTtag _ = Nothing  
