
module Nostr.Filter where 

import Prelude as P 
import Data.Aeson as J
import Data.Aeson.Types
import Data.Aeson.Key
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics 
import Nostr.Event 
import Nostr.Keys

unString :: Value -> Text
unString (String t) = t 
unString _ = undefined 

matchFx :: Event -> [Filter] -> Bool 
matchFx = any . matchF 

matchF :: Event -> Filter -> Bool
matchF e' (Filter i a k e p s u _ _  ) = all id . catMaybes $ 
    [ matchM e' <$> i  
    , matchM e' <$> a
    , matchM e' <$> k
    , matchM e' <$> e
    , matchM e' <$> p
    , matchM e' <$> s
    , matchM e' <$> u
    ] 
     
data Filter = Filter { 
      idsF :: Maybe Ids
    , authorsF :: Maybe Authors
    , kindsF :: Maybe Kinds 
    , etagF :: Maybe ETagM
    , ptagF :: Maybe PTagM
    , sinceF :: (Maybe Since)
    , untilF :: Maybe Until  
    , aztagF :: [Tag]
    , limitF :: Maybe Limit   
    } deriving (Show, Eq, Generic)
    
emptyF :: Filter
emptyF = 
     Filter 
        Nothing Nothing Nothing 
        Nothing Nothing Nothing 
        Nothing []      Nothing 
    
instance ToJSON Filter where 
    toJSON (Filter i a k e p s u tx ml) = object . catMaybes $ 
        [ toKv <$> i 
        , toKv <$> a 
        , toKv <$> k
        , toKv <$> e
        , toKv <$> p
        , toKv <$> s
        , toKv <$> u
        , toKv <$> ml
        ] <> P.map (Just . toKv) tx

newtype Ids = Ids [Text] deriving (Show, Eq, Generic)
newtype Authors = Authors [Text] deriving (Show, Eq, Generic)
newtype Kinds = Kinds [Int]   deriving (Show, Eq, Generic) 
newtype ETagM = ETagM [Hex32] deriving (Show, Eq, Generic)
newtype PTagM = PTagM [Hex32] deriving (Show, Eq, Generic)
newtype Since = Since Integer deriving (Show, Eq, Generic)
newtype Until = Until Integer deriving (Show, Eq, Generic)
newtype Limit = Limit Int     deriving (Show, Eq, Generic)

instance FromJSON Ids
instance FromJSON Authors
instance FromJSON Kinds
instance FromJSON ETagM
instance FromJSON PTagM
instance FromJSON Since
instance FromJSON Until
instance FromJSON Limit

class Matchable a where
    matchM :: Event -> a -> Bool

instance Matchable Ids where 
    matchM e (Ids tx) = any (flip T.isPrefixOf (unString . toJSON . eid $ e)) tx

instance Matchable Authors where
    matchM e (Authors ax) = any 
        (flip T.isPrefixOf (unString . toJSON . pubkey . con $ e)) ax  

instance Matchable Kinds where 
    matchM e (Kinds kx) = any (== (kind . con $ e)) kx

instance Matchable ETagM where 
    matchM e (ETagM ex) = any (flip elem etags) ex
        where 
        etags = map (\(ETag e _ _ )-> e) . filter isEtag . tags . con $ e
        isEtag (ETag{}) = True
        isEtag _ = False 
          
instance Matchable PTagM where
    matchM e (PTagM px) = any (flip elem ptags) px
        where 
        ptags = map (\(PTag p _ _) -> p) . filter isPtag . tags . con $ e
        isPtag (PTag{}) = True
        isPtag _ = False   

instance Matchable Since where 
    matchM e (Since t) = (created_at . con $ e) > t

instance Matchable Until where 
    matchM e (Until t) = (created_at . con $ e) < t

class Keyable a where 
    toKv :: KeyValue x => a -> x

instance Keyable Ids where 
    toKv (Ids t)     = "ids" .= t
    
instance Keyable Authors where 
    toKv (Authors a) = "authors" .= a
    
instance Keyable Kinds where 
    toKv (Kinds k)   = "kinds" .= k
    
instance Keyable ETagM where 
    toKv (ETagM e)   = "#e" .= e
    
instance Keyable PTagM where 
    toKv (PTagM p)   = "#p" .= p
    
instance Keyable Since where 
    toKv (Since s)   = "since" .= s
    
instance Keyable Until where 
    toKv (Until u)   = "until" .= u

instance Keyable Limit where 
    toKv (Limit l) = "limit" .= l

instance Keyable Tag where 
    toKv (AZTag l x) = (fromText $ "#" <> T.singleton l) .= x
    toKv _ = undefined  
    
instance FromJSON Filter where 
    parseJSON = withObject "filter" buildFilter 
      where 
      buildFilter :: Object -> Parser Filter
      buildFilter o = Filter 
          <$> (o .:? "ids")    
          <*>  (o .:? "authors")
          <*>  (o .:? "kinds")
          <*>   o .:? "#e"
          <*>   o .:? "#p"
          <*>   o .:? "since"
          <*>   o .:? "until"
          <*>   getTags o
          <*>   o .:? "limit"       
    
getTags :: Object -> Parser [Tag]
getTags a =  
    let letters = [x | x <- ['a'..'z'] ++ ['A'..'Z'], notElem x ("epEP" :: String)]
        ags = P.map (checkTag a) letters
    in catMaybes <$> (sequenceA $ ags) 

checkTag :: Object -> Char -> Parser (Maybe Tag)
checkTag o x = (AZTag x <$>) <$> (o .:? (fromText $ "#" <> T.singleton x))
