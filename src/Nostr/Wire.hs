module Nostr.Wire where 

import Prelude as P 
import Data.Text as T 
import Data.Aeson as J
import qualified Data.Vector as V
import GHC.Generics 
import Nostr.Filter
import Nostr.Event 
import Nostr.Keys

-- | client -> server 
data Up
    = Submit Event
    | Subscribe Text [Filter]
    | Auth Event
    | End Text
    deriving (Show, Eq, Generic)

-- | server -> client
data Down
    = See Text Event
    | Challenge Text
    | Ok Hex32 Bool WhyNot
    | Live Text
    | Notice Text
    deriving (Eq, Show)

data WhyNot 
    = None 
    | ServerErr Text 
    | Invalid Text 
    | Pow Text 
    | Duplicate Text 
    | Block Text 
    | RateLimit Text 
    | Restrict Text
    deriving (Eq, Show)

instance ToJSON WhyNot where 
    toJSON None          = String ""
    toJSON (ServerErr t) = String $ "error:" <> t
    toJSON (Invalid t)   = String $ "invalid:" <> t
    toJSON (Pow t)       = String $ "pow:" <> t
    toJSON (Duplicate t) = String $ "duplicate:" <> t
    toJSON (Block t)     = String $ "blocked:" <> t
    toJSON (RateLimit t) = String $ "rate-limited:" <> t
    toJSON (Restrict t)  = String $ "restricted:" <> t 

instance FromJSON WhyNot where 
    parseJSON = withText "why not?" $ \t -> 
        pure . parseWhyNot $ t

parseWhyNot :: Text -> WhyNot
parseWhyNot (t) = 
    case f of 
        "" -> None
        "error" -> ServerErr t'
        "invalid" -> Invalid t'
        "pow" -> Pow t'
        "duplicate" -> Duplicate t'
        "blocked" -> Block t'
        "rate-limited" -> RateLimit t'
        "restricted" -> Restrict t'
        _ -> error "not sure" 
    where 
    (f , fx) = case T.split (== ':') t of 
        [] -> ("~", [])
        f' : fx' -> (f', fx')
        
    t' = T.intercalate ":" fx
        
instance FromJSON Up where 
    parseJSON = withArray "up" \a -> 
        case V.head a of 
            "EVENT" -> Submit <$> parseJSON (V.last a)
            "REQ" -> Subscribe <$> parseJSON (a V.! 1) 
                               <*> sequenceA  
                                   (V.foldr ((:) . parseJSON) [] (V.drop 2 a)) 
            "CLOSE" -> End <$> parseJSON (V.last a)
            "AUTH" -> Auth <$> parseJSON (V.last a)
            _ -> fail "unimpl parseJSON"
            
instance ToJSON Up where 
    toJSON (Submit e) = toJSON [
          String "EVENT"
        , toJSON e
        ]
    toJSON (Subscribe s fx) = toJSON $ [
          String "REQ"
        , String s 
        ] <> P.map toJSON fx
    toJSON (End s) = toJSON [String "CLOSE", String s]
    toJSON (Auth e) = toJSON [String "AUTH", toJSON e]

instance FromJSON Down where 
    parseJSON = withArray "down" \a -> 
        case (V.head a, V.head . V.tail $ a) of 
            ("EVENT", String s) -> See s <$> parseJSON (V.last a)        
            ("EOSE", String s) -> pure $ Live s
            ("NOTICE", String n) -> pure $ Notice n
            ("AUTH", String t) -> pure $ Challenge t
            ("OK", _ ) -> Ok <$> parseJSON (a V.! 1)
                             <*> parseJSON (a V.! 2)
                             <*> parseJSON (a V.! 3)
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
    toJSON (Challenge t) = toJSON [String "AUTH", toJSON t]
    toJSON (Notice n) = toJSON [String "NOTICE", String n]


    