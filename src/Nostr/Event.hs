
module Nostr.Event where 

import qualified Data.ByteString as BS
import Data.ByteString.Base16 as Hex
import Data.Aeson as J
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Char
import GHC.Generics 
import Foreign.Marshal.Alloc
import qualified Crypto.Hash.SHA256 as SHA256
import Secp256k1.Internal
import Nostr.Keys
import Data.Int
import Data.Text.Encoding
import Data.Aeson.Text

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
                     else signE kp keyless -- signE kp keyless 
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
    | PTag Hex32 (Maybe Text) (Maybe Text) 
    | ATag ARef (Maybe Text)
    | Nonce Int Int
    | Chal Text
    | Expiry Int64
    | AZTag Char Text 
    | Tag  Array
    deriving (Eq, Show, Generic)
data Marker = Reply' | Root' | Mention'
    deriving (Eq, Show, Read, Generic)
    
data ARef = ARef Int Hex32 (Maybe Text) 
                   deriving (Eq, Show, Generic)

instance FromJSON ARef where 
    parseJSON = withText "replaceable" \t ->  
        case T.splitOn ":" t of 
            [a, b] -> makeAR a b Nothing
            [a, b, ""] -> makeAR a b Nothing
            [a, b, c] -> makeAR a b (Just c)
            _ -> fail "Invalid ARef"
        where makeAR a b mc = flip (ARef (read $ T.unpack a)) mc  
                              <$> parseJSON (String b)

instance ToJSON ARef where 
    toJSON (ARef (wq -> k) (wq -> p) Nothing) = String $ 
        k <> ":" <> p <> ":" 
    toJSON (ARef (wq -> k) (wq -> p) (Just d)) = String $ 
        k <> ":" <> p <> ":" <> d 


getNonce :: Value -> Parser Int
getNonce v = case v of  
    String s -> pure .  (read :: String -> Int) . unpack $ s 
    Number n -> pure . truncate $ n
    _ -> fail "nonce"
    
    
instance FromJSON Tag where 
    parseJSON = J.withArray "tag" \a -> do 
        let n1 = getNonce $ a V.! 1 
            n2 = getNonce $ a V.! 2
        case a V.! 0 of 
            String "e" -> ETag <$> parseJSON (a V.! 1)
                               <*> traverse parseJSON (a V.!? 2)
                               <*> traverse parseJSON (a V.!? 3)
            String "p" -> PTag <$> parseJSON (a V.! 1) 
                               <*> traverse parseJSON (a V.!? 2) 
                               <*> traverse parseJSON (a V.!? 3)
            String "a" -> ATag <$> parseJSON (a V.! 1)
                               <*> traverse parseJSON (a V.!? 2)
            String "nonce" -> Nonce <$> n1 <*> n2 
            String "challenge" -> Chal <$> parseJSON (a V.! 1)
            String "expiration" -> Expiry . fromIntegral <$> n1
            String (isAZ -> True) -> AZTag  <$> parseJSON (a V.! 0)
                                            <*> parseJSON (a V.! 1)
            _ -> pure $ Tag a
            
isAZ :: Text -> Bool
isAZ t = case T.uncons t of 
    Just (a, (T.null -> True)) -> isAlpha a  
    _ -> False 
            
instance ToJSON Tag where 
    toJSON (ETag i mr mm) = toJSON $ case (mr, mm) of 
        (Just r, Just m) -> ee <> [toJSON r, toJSON m]
        (Nothing, Just m) -> ee <> [String "", toJSON m]
        (Just r, Nothing) -> ee <> [toJSON r]
        (Nothing, Nothing) -> ee
        where 
        ee = [String "e", toJSON i]
    toJSON (PTag i mr mm) = toJSON case (mr, mm) of
        (Just r, Just p) -> pp <> [toJSON r, toJSON p]
        (Just r, Nothing) -> pp <> [toJSON r]
        (Nothing, Just p) -> pp <> [String "", toJSON p]
        (Nothing, Nothing) -> [String "p", toJSON i]
        where 
        pp = [String "p", toJSON i]
    toJSON (ATag ref mr) = toJSON case mr of 
        Just r -> aa <> [String r]
        Nothing -> aa
        where 
        aa = [String "a", toJSON ref]
    toJSON (Expiry x) = toJSON [String "expiration", toJSON x]
    toJSON (Tag a) = toJSON a                       
    toJSON (Nonce a i) = toJSON [String "nonce", toJSON a, toJSON i ]
    toJSON (Chal t) = toJSON [String "challenge", toJSON t]
    toJSON (AZTag az val) = toJSON [String . singleton $ az, String val]
  
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


wq :: ToJSON a => a -> Text 
wq (toJSON -> a) = case a of 
    String s -> s 
    _ -> (LT.toStrict . encodeToLazyText) a

qw :: FromJSON a => Text -> Maybe a
qw = J.decode . BS.fromStrict . encodeUtf8 
