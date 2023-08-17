module Nostr.Relay where 

import Data.Aeson as J
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text (Text)
import Data.ByteString.Base16 as Hex
import GHC.Generics 
import Nostr.Event 

matchF :: Event -> Filter -> Bool
matchF e (Filter mx _) = all (matchM e) mx 

matchM :: Event -> Match -> Bool
matchM e (Ids tx) = undefined 
data Filter = Filter [Match] (Maybe Int) deriving (Eq, Show)
data Match = 
      Ids [Text]
    | Authors [Text]
    | Kinds [Int]
    | ETagM [Hex32]
    | PTagM [Hex32]
    | Since Int
    | Until Int
    deriving (Eq, Show, Generic)

toKv :: KeyValue x => Match -> x
toKv (Ids t)     = "ids" .= t
toKv (Authors a) = "authors" .= a
toKv (Kinds k)   = "kinds" .= k
toKv (ETagM e)   = "#e" .= e
toKv (PTagM p)   = "#p" .= p
toKv (Since s)   = "since" .= s
toKv (Until u)   = "until" .= u

instance ToJSON Filter where 
    toJSON (Filter mx (Just l)) = object $ map toKv mx <> [("limit" .= l)] 
    toJSON (Filter mx Nothing) = object $ map toKv mx
instance ToJSON Match  

instance FromJSON Filter where 
    parseJSON = withObject "filter" buildFilter 
      where 
      buildFilter :: Object -> Parser Filter
      buildFilter o = Filter . catMaybes <$> matches o <*> limit o 
      matches :: Object -> Parser [Maybe Match] 
      matches o = sequenceA [ 
            fmap (fmap Ids) (o .:? "ids")
          , fmap (fmap Authors) (o .:? "authors")
          , fmap (fmap Kinds) (o .:? "kinds")
          , fmap (fmap ETagM) $ o .:? "#e"
          , fmap (fmap PTagM) $ o .:? "#p"
          , fmap (fmap Since) $ o .:? "since"
          , fmap (fmap Until) $ o .:? "until"
          ]
      limit :: Object -> Parser (Maybe Int)
      limit o = o .:? "limit"

instance FromJSON Match

data Up
    = Submit Event
    | Subscribe Text [Filter]
    | Close Text
    deriving (Generic)

data Down
    = See Text Event
    | Live Text
    | Notice Text

instance FromJSON Up where 
    parseJSON = withArray "up" \a -> 
        case V.head a of 
            "EVENT" -> Submit <$> parseJSON (V.last a)
            "REQ" -> Subscribe <$> parseJSON (a V.! 1) 
                               <*> traverse id  
                                   (V.foldr ((:) . parseJSON) [] (V.drop 2 a)) 
            "CLOSE" -> Close <$> parseJSON (V.last a)
            _ -> fail "unimpl parseJSON"
            
instance ToJSON Up where 
    toJSON (Submit e) = toJSON [
          String "EVENT"
        , toJSON e
        ]
    toJSON (Subscribe s fx) = toJSON $ [
          String "REQ"
        , String s 
        ] <> map toJSON fx
    toJSON (Close s) = toJSON [String "CLOSE", String s]

instance FromJSON Down where 
    parseJSON = withArray "down" \a -> 
        case (V.head a, V.head . V.tail $ a) of 
            ("EVENT", String s) -> See s <$> parseJSON (V.last a)        
            ("EOSE", String s) -> pure $ Live s
            ("NOTICE", String n) -> pure $ Notice n
            _ -> fail . show $ a
            

instance ToJSON Down where 
    toJSON (See s e) = toJSON [
          String "EVENT"
        , String s
        , toJSON e
        ]
    toJSON (Live s) = toJSON [String "EOSE", String s]
    toJSON (Notice n) = toJSON [String "NOTICE", String n]
