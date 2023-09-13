
module Nostr.Event where 

import qualified Data.ByteString as BS
import Data.ByteString.Base16 as Hex
import Data.Aeson as J
import qualified Data.Vector as V
import Data.Text (Text)
import GHC.Generics 
import Foreign.Marshal.Alloc
import qualified Crypto.Hash.SHA256 as SHA256
import Secp256k1.Internal
import Nostr.Keys

verifyE :: Event -> IO Bool 
verifyE Event{..}  
    | idE con == eid = do 
        signPub <- parsePub . pubkey $ con
        (msg', 32) <- getPtr (un32 eid) -- \(msg', 32) ->  
        (sig', 64) <- getPtr (un64 sig) 
        (pub', 64) <- getPtr (un64 signPub)
        (== 1) <$> schnorrSignatureVerify ctx sig' msg' 32 pub' 
    | otherwise = pure False 

signE :: Hex96 -> Keyless -> IO Event
signE kp keyless = do
    content <- keyless <$> exportPub kp
    let eid = idE content
    (priv, 96) <- getPtr (un96 kp)
    sig <- mallocBytes 64
    (msg, 32) <- getPtr . un32 $ eid 
    (salt, 32) <- genSalt
    ret <- schnorrSign ctx sig msg priv salt
    case ret of 
        1 -> do 
            sig' <- Hex64 <$> packPtr (sig, 64)
            let newE = Event eid sig' content
            trust <- verifyE newE
            if trust then pure newE
                     else  -- error "created invalid ? " 
                        signE kp keyless -- signE kp keyless 
        _ -> free sig >> error "schnorrSign error"
             
idE :: Content -> Hex32
idE Content{..} = Hex32 
    . Hex.decodeLenient 
    . Hex.encode 
    . SHA256.hash 
    . BS.toStrict 
    . J.encode $ 
        [ Number 0
        , toJSON pubkey
        , Number $ fromIntegral created_at
        , Number $ fromIntegral kind
        , toJSON tags
        , String content
        ]
 
data Event = Event {
      eid :: Hex32
    , sig :: Hex64
    , con :: Content
    } deriving (Eq, Show)

data Content = Content {
      kind       :: Int
    , tags       :: [Tag]
    , content    :: Text
    , created_at :: Integer
    , pubkey     :: Hex32
    } deriving (Eq, Show, Generic)
instance ToJSON Content
instance FromJSON Content

type Keyless = (Hex32 -> Content)

instance ToJSON Event where 
    toJSON (Event i s (Content{..})) = object [
          "id"         .= i
        , "sig"        .= s      
        , "pubkey"     .= pubkey
        , "created_at" .= created_at
        , "kind"       .= kind
        , "tags"       .= tags 
        , "content"    .= content
        ]  

instance FromJSON Event where 
    parseJSON = withObject "event" \o ->  
        Event <$> o .: "id" 
              <*> o .: "sig"
              <*> (Content 
                  <$> o .: "kind"
                  <*> o .: "tags" 
                  <*> o .: "content"
                  <*> o .: "created_at" 
                  <*> o .: "pubkey" 
                  )
data Tag = 
      ETag Hex32 (Maybe Text) (Maybe Marker)
    | PTag Hex32 (Maybe Text)
    | Nonce Int Int
    | Tag  Array
    deriving (Eq, Show, Generic)
data Marker = Reply' | Root' | Mention'
    deriving (Eq, Show, Read, Generic)
    
instance FromJSON Tag where 
    parseJSON = J.withArray "tag" \a -> do 
        let tag = a V.! 0
            evId = parseJSON (a V.! 1)
            rel = traverse parseJSON $ a V.!? 2
            mar = traverse parseJSON $ a V.!? 3
        case tag of 
            String "e" -> ETag <$> evId <*> rel <*> mar 
            String "p" -> PTag <$> evId <*> rel
            _ -> pure $ Tag a
instance ToJSON Tag where 
    toJSON (ETag i mr mm) = toJSON $ case (mr, mm) of 
        (Just r, Just m) -> ee <> [toJSON r, toJSON m]
        (Nothing, Just m) -> ee <> [String "", toJSON m]
        (Just r, Nothing) -> ee <> [toJSON r]
        (Nothing, Nothing) -> ee
        where 
        ee = [String "e", toJSON i]
    toJSON (PTag i mr) = toJSON case mr of 
        Just r -> [String "p", toJSON i, toJSON r]
        Nothing -> [String "p", toJSON i]
    toJSON (Tag a) = toJSON a                       
    toJSON (Nonce a i) = toJSON [String "nonce", toJSON a, toJSON i ]
  
instance FromJSON Marker where 
    parseJSON = withText "marker" \case 
        "reply" -> pure Reply'
        "root"  -> pure Root'
        "mention" -> pure Mention' 
        _ -> fail "invalid marker"
instance ToJSON Marker where 
    toJSON Reply' = String "reply"
    toJSON Root' = String "root"
    toJSON Mention' = String "mention"

