
module Nostr.Kinds where

import Nostr.Event
import Data.Aeson
-- import Data.Text.Encoding
-- import Data.ByteString.Lazy
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text (Text)

kindE :: Event -> Kind
kindE e = case kind . con $ e of 
    0 -> kind0 e
    1 -> kind1 e
    _ -> Rest e 

data Kind = 
      Kind0 Text Text Text
    | Kind1
    | Rest Event

kind0 :: Event -> Kind
kind0 (Event _ _ (Content {..})) = 
    case (decode . encodeUtf8 . fromStrict) content of
        Just (Profile a b c) -> Kind0 a b c 
        _ -> error "zero"

kind1 :: Event -> Kind
kind1 (Event _ _ (Content {..})) = Kind1

data Profile = Profile Text Text Text

instance FromJSON Profile where 
    parseJSON = withObject "kind0" \o -> 
        Profile <$> (o .: "name")
                <*> (o .: "about")
                <*> (o .: "picture")
