module Nostr.Wire where 

import Data.Aeson as J
import qualified Data.Vector as V
import Data.Text (Text)
import GHC.Generics 
import Nostr.Filter
import Nostr.Event 
import Nostr.Keys

-- | client -> server 
data Up
    = Submit Event
    | Subscribe Text [Filter]
    | End Text
    deriving (Generic)

-- | server -> client
data Down
    = See Text Event
    | Ok Hex32 Bool WhyNot
    | Live Text
    | Notice Text

data WhyNot 
    = None 
    | ServerErr Text 
    | Invalid Text 
    | Pow Text 
    | Duplicate Text 
    | Block Text 
    | RateLimit Text 

instance ToJSON WhyNot where 
    toJSON None          = String ""
    toJSON (ServerErr t) = String $ "error: " <> t
    toJSON (Invalid t)   = String $ "invalid: " <> t
    toJSON (Pow t)       = String $ "pow: " <> t
    toJSON (Duplicate t) = String $ "duplicate: " <> t
    toJSON (Block t)     = String $ "blocked: " <> t
    toJSON (RateLimit t) = String $ "rate-limited: " <> t

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
    toJSON (Ok eid success reason) = toJSON 
        [String "OK", toJSON eid, toJSON success, toJSON reason]
    toJSON (Notice n) = toJSON [String "NOTICE", String n]


