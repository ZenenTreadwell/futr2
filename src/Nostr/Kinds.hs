
module Nostr.Kinds where

import Nostr.Event
import Nostr.Direct 
import Nostr.Keys
import Data.Aeson
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (delete, toList)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)
import Text.Regex.TDFA
import Text.URI
import Control.Monad.State
import Data.Maybe

kindE :: Event -> Kind
kindE e = case kind . con $ e of 
    0 -> kind0 e 
    1 -> kind1 e
    4 -> Kind4 $ flip decryptE e 
    _ -> Rest . con $ e 

data Kind = 
      Kind0 (Maybe Profile) 
    | Kind1 { 
              imglinks :: [URI]
            , otherlinks :: [URI]
            , contentk1 :: Text
            , tagsk1 :: [Tag]
            --  
            }
    | Kind4 (Hex96 -> IO (Maybe Text))
    | Rest Content
    -- deriving (Eq) can't for function pointers

kind0 :: Event -> Kind
kind0 (Event _ _ (Content {..})) = Kind0 
    ((decode . encodeUtf8 . fromStrict) content :: Maybe Profile)

data Profile = Profile Text Text Text (Maybe Text) [(Text, Text)]
    deriving Eq

instance FromJSON Profile where 
    parseJSON = withObject "kind0" \o ->  
    
        let toT (toText -> t, (toStrict . decodeUtf8 . encode) -> t2) = (t,t2) 
            additional = map toT . toList 
                             . delete "picture" 
                             . delete "about" 
                             . delete "name"
                             . delete "banner"
                             $ o
        in
        Profile <$> (o .: "name")
                <*> (o .: "about")
                <*> (o .: "picture")
                <*> (o .:? "banner")
                <*> pure additional

-- these REGEXES (XXX PARSING)
kind1 :: Event -> Kind 
kind1 (Event _ _ (Content {content})) =
    let 
    (noimglinks, mapMaybe mkURI -> imglinks) = content ~=~ "http.+(jpg|png)" 
    (nolinks, mapMaybe mkURI -> othlinks) = noimglinks ~=~ "http[^[:space:]]+" 
    (nohtgs, map ttxn -> hashtags)  = nolinks ~=~ "#[^[:space:]]+" 

    --- XXX nip19 / nip21
    (nonoes, map ptxn . mapMaybe (xnpub) -> mentions) = nohtgs ~=~ "npub[^[:space:]]+"
    
    in 
    Kind1 
        imglinks 
        othlinks
        nonoes 
        (mentions <> hashtags) 
        
    where
    ptxn :: Hex32 -> Tag
    ptxn = flip (`PTag` Nothing) Nothing 

    ttxn :: Text -> Tag
    ttxn = AZTag 't'
     
getContentTags :: Event -> [Tag]
getContentTags (kindE -> Kind1{tagsk1}) = tagsk1 
getContentTags _ = [] 
                
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


