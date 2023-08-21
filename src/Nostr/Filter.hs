module Nostr.Filter where 

import Data.Aeson as J
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics 
import Nostr.Event 

unString :: Value -> Text
unString (String t) = t 
unString _ = undefined 

matchFx :: Event -> [Filter] -> Bool 
matchFx = any . matchF 

matchF :: Event -> Filter -> Bool
matchF e (Filter mx _) = all (matchM e) mx 

matchM :: Event -> Match -> Bool
matchM e (Ids tx) = any (flip T.isPrefixOf (unString . toJSON . eid $ e)) tx

matchM e (Authors ax) = any (flip T.isPrefixOf (unString . toJSON . pubkey . con $ e)) ax  

matchM e (Kinds kx) = any (== (kind . con $ e)) kx

matchM e (ETagM ex) = any (flip elem etags) ex
    where 
    etags = map (\(ETag e _ _ )-> e) . filter isEtag . tags . con $ e
    isEtag (ETag{}) = True
    isEtag _ = False   

matchM e (PTagM px) = any (flip elem ptags) px
    where 
    ptags = map (\(PTag p _) -> p) . filter isPtag . tags . con $ e
    isPtag (PTag{}) = True
    isPtag _ = False   
    
matchM e (Since t) = (created_at . con $ e) > t
matchM e (Until t) = (created_at . con $ e) < t
     
data Filter = Filter [Match] (Maybe Int) deriving (Eq, Show)
data Match = 
      Ids [Text]
    | Authors [Text]
    | Kinds [Int]
    | ETagM [Hex32]
    | PTagM [Hex32]
    | Since Integer
    | Until Integer
    deriving (Eq, Show, Generic)

instance ToJSON Filter where 
    toJSON (Filter mx (Just l)) = object $ map toKv mx <> [("limit" .= l)] 
    toJSON (Filter mx Nothing) = object $ map toKv mx
toKv :: KeyValue x => Match -> x
toKv (Ids t)     = "ids" .= t
toKv (Authors a) = "authors" .= a
toKv (Kinds k)   = "kinds" .= k
toKv (ETagM e)   = "#e" .= e
toKv (PTagM p)   = "#p" .= p
toKv (Since s)   = "since" .= s
toKv (Until u)   = "until" .= u
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
