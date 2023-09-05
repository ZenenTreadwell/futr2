module Nostr.Wire where 

import Data.Aeson as J
import qualified Data.Vector as V
import Data.Text (Text)
import GHC.Generics 
import Nostr.Filter
import Nostr.Event 

-- | client -> server 
data Up
    = Submit Event
    | Subscribe Text [Filter]
    | End Text
    deriving (Generic)

-- | server -> client
data Down
    = See Text Event
    | Live Text
    | Notice Text

instance FromJSON Up where 
    parseJSON = withArray "up" \a -> 
        case V.head a of 
            "EVENT" -> Submit <$> parseJSON (V.last a)
            "REQ" -> Subscribe <$> parseJSON (a V.! 1) 
                               <*> sequenceA  
                                   (V.foldr ((:) . parseJSON) [] (V.drop 2 a)) 
            "CLOSE" -> End <$> parseJSON (V.last a)
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
    toJSON (End s) = toJSON [String "CLOSE", String s]

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
