
module Nostr.Event where 

import qualified Data.ByteString as BS
import Data.ByteString.Base16 as Hex
import Data.Aeson as J
import qualified Data.Vector as V
import Data.Text as T
import Data.Char
import GHC.Generics 
import Foreign.Marshal.Alloc
import qualified Crypto.Hash.SHA256 as SHA256
import Secp256k1.Internal
import Nostr.Keys

-- broadcastE :: Event -> IO (TChan Event)
broadcastE _ = undefined 

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
    | PTag Hex32 (Maybe Text)
    | Nonce Int Int
    | Chal Text
    | AZTag Char Text 
    | Tag  Array
    deriving (Eq, Show, Generic)
data Marker = Reply' | Root' | Mention'
    deriving (Eq, Show, Read, Generic)
    
instance FromJSON Tag where 
    parseJSON = J.withArray "tag" \a -> do 
        case a V.! 0 of 
            String "e" -> ETag <$> parseJSON (a V.! 1)
                               <*> traverse parseJSON (a V.!? 2)
                               <*> traverse parseJSON (a V.!? 3)
            String "p" -> PTag <$> parseJSON (a V.! 1) 
                               <*> traverse parseJSON (a V.!? 2) 
            String "nonce" -> Nonce <$> parseJSON (a V.! 1) 
                                    <*> parseJSON (a V.! 2) 
            String "challenge" -> Chal <$> parseJSON (a V.! 1)
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
    toJSON (PTag i mr) = toJSON case mr of 
        Just r -> [String "p", toJSON i, toJSON r]
        Nothing -> [String "p", toJSON i]
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

-- "[\"EVENT\",\"a\",
-- c{\"id\":\"000cc40d5beed48746ca67a9fada6c4878e9172a3404af008b1ce8f8492e32cd\",
-- \"pubkey\"
-- :\"f33207c9e96df017186a1cf4245c8ad679d8d1be4b037a72e82188f4deade523\",i
-- \"created_at\":1696219797,\"kind\":1,\"tags\":


-- tags = [
--     ["t","Presse"],
--     [\"t\",\"lematin\"],
    
--     [\"t\",\"Suisse\"],
    
--     [\"proxy\",\"https://www.lematin.ch/story/egypte-enorme-incendie-a-ismailiya-sur-le-canal-de-suez-341070967425\",\"rss\"],
    
--     [\"nonce\",\"1204\",\"10
-- ,\"content\":\"Le Matin (Suisse)\\n\\n\195\137norme incendie \195\160 Isma\195\175liya sur le canal de Suez\\n\\nUn immense feu s\226\128\153est d\195\169clench\195\169 au si\195\168ge de la Direction de la S\195\187ret\195\169 avant l\226\128\153aube lundi. Aucun bilan n\226\128\153a \195\169t\195\169 communiqu\195\169 pour le moment.\\n\\nhttps://www.lematin.ch/341070967425\\n\\n#Presse #lematin #Suisse\",\"sig\":\"ac20ab965f232216eca838c4a48956c33e84f72add09ee1c77a16ca4ffbe58cb8d8c2be165539f82498fb0caca751194a4b403c0d4d37d35826d61488b34fe4e\"}]"
