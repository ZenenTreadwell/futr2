module Nostr.Auth where 

import Prelude as P 
import Data.List as L
import Nostr.Event 
import Nostr.Keys 
import Text.URI
import Data.Bits
import Data.Text (Text)
import Data.Aeson
import Data.Time.Clock.POSIX
import Data.Vector as V
import Data.ByteString as BS
import Control.Monad.State
import Control.Monad.Trans.Maybe

authenticate :: Hex96 -> URI -> Hex32 -> IO Event
authenticate kp uri t = do 
    now <- round <$> getPOSIXTime 
    let relayT = V.fromList [String "relay", String $ render uri]
    let answerT = Chal t
    let keyless = Content 22242 [Tag relayT, answerT] "" now  
    signE kp keyless

validate :: Event -> Hex32 -> MaybeT IO Hex32 
validate e@(Event i s (Content{..})) challenge = do 
    Just (Chal c) <- pure $ L.find isChal tags
    v <- liftIO $ verifyE e
    liftIO $ print "checkiddy" >> print (c == challenge) >> print v
    if v && c == challenge 
        then pure pubkey    
        else mzero
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
difficulty (Hex32 bs) = go 0 bs 
    where 
    go count bs 
        | BS.null bs      = count
        | BS.head bs == 0 = go (count + 8) (BS.tail bs)
        | otherwise = count + lead 7 0 (BS.head bs)
    lead i count b 
        | testBit b i = count   
        | otherwise   = lead (i - 1) (count + 1) b

    

