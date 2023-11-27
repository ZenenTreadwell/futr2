
module Nostr.Kinds where

import Nostr.Event
import Data.Aeson
-- import Data.Text.Encoding
-- import Data.ByteString.Lazy
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text (Text)

import Text.Regex.TDFA
import Control.Monad.State

kindE :: Event -> Kind
kindE e = case kind . con $ e of 
    0 -> kind0 e
    1 -> kind1 e
    _ -> Rest e 

data Kind = 
      Kind0 Text Text Text
    | Kind1 [Text] [Text] Text
    | Rest Event

kind0 :: Event -> Kind
kind0 (Event _ _ (Content {..})) = 
    case (decode . encodeUtf8 . fromStrict) content of
        Just (Profile a b c) -> Kind0 a b c 
        _ -> error "zero"

kind1 :: Event -> Kind
kind1 (Event _ _ (Content {..})) =
    let 
    (nolinks, links) = content ~=~ "http.+(jpg|png)" 
    (_, hashtags)  = nolinks ~=~ "#[^:space:]+" 
    in Kind1 links hashtags nolinks
    
        
data Profile = Profile Text Text Text

instance FromJSON Profile where 
    parseJSON = withObject "kind0" \o -> 
        Profile <$> (o .: "name")
                <*> (o .: "about")
                <*> (o .: "picture")
                

type RegT = State (Text, [Text], Text) (Text, [Text], Text)

extractReg :: Text -> RegT  
extractReg reg  = do 
    (tb, tlx, ta) <- get  
    case ta =~ reg :: (Text, Text, Text) of 
        (t, "", "") -> pure (tb <> t, tlx, "")
        (t, ll, "") -> pure (tb <> t, ll : tlx, "")
        (blt, ll, btl) -> put (tb <> blt, ll : tlx, btl) 
                              >> extractReg reg
(~=~) :: Text -> Text -> (Text, [Text])
star ~=~ reg = x $ evalState (extractReg reg) ("", [], star) 
    where 
    x (a,b,_) = (a,b)
