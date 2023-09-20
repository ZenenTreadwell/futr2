module Nostr.Auth where 

import Prelude as P 
import Data.List as L
import Nostr.Event 
import Nostr.Keys 
import Text.URI
import Data.Bits
import Data.Text (Text)
import Data.Word
import Data.Aeson
import Data.Time.Clock.POSIX
import Data.Vector as V
import Data.ByteString as BS
import Control.Monad.State
import Control.Monad.Trans.Maybe

authenticate :: Hex96 -> URI -> Text -> IO Event
authenticate kp uri t = do 
    now <- round <$> getPOSIXTime 
    let relayT = V.fromList [String "relay", String $ render uri]
    let answerT = Chal t
    let keyless = Content 22242 [Tag relayT, answerT] "" now  
    signE kp keyless

validate :: Event -> Text -> MaybeT IO Hex32 
validate e@(Event _ _ (Content{..})) challenge = do 
    22242 <- pure kind
    Just (Chal c) <- pure $ L.find isChal tags
    liftIO $ print c
    liftIO $ print challenge
    True <- pure $ c == challenge
    True <- lift $ verifyE e
    pure pubkey
    where
    isChal :: Tag -> Bool
    isChal (Chal _) = True
    isChal _ = False 

mine :: Int -> Content -> (Keyless, Int)  
mine target c@(Content{tags}) = evalState miner 0 
    where 
    miner :: State Int (Keyless, Int) 
    miner = do 
        nonce <- state (\n -> (n,n+1)) 
        let c' = c { tags = setNonce nonce tags} 
        let diff' = difficulty . idE $ c'
        if diff' >= target
            then pure (\_ -> c', diff')
            else miner 
    setNonce :: Int -> [Tag] -> [Tag]
    setNonce n tx = (:) (Nonce n target) (P.filter (not . isNonce)  tx) 
    isNonce :: Tag -> Bool
    isNonce (Nonce _ _) = True
    isNonce _ = False

judge :: Int -> Event -> Bool 
judge target = (>= target) . difficulty . eid  

difficulty :: Hex32 -> Int 
difficulty (Hex32 b) = go 0 b 
    where 

    go i bs 
        | BS.null bs      = i
        | BS.head bs == 0 = go (i + 8) (BS.tail bs)
        | otherwise = i + lead (BS.head bs)

    lead = lo 0 7 

    lo x o y   
        | testBit y o = x   
        | otherwise   = lo (x + 1) (o - 1) y

    

