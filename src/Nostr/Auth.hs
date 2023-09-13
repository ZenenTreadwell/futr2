module Nostr.Auth where 
import Data.Maybe
import Prelude as P 
import Nostr.Event 
import Nostr.Keys 
import Text.URI
import Data.Word
import Data.Bits
import Data.Text (Text)
import Data.Aeson
import Data.Time.Clock.POSIX
import Data.Vector as V
import Data.ByteString as BS
import Control.Monad.State

authenticate :: Hex96 -> URI -> Text -> IO Event
authenticate kp uri t = do 
    now <- round <$> getPOSIXTime 
    let relayT = V.fromList [String "relay", String $ render uri]
    let answerT = V.fromList [String "challenge", String t]
    let keyless = Content 22242 [Tag relayT, Tag answerT] "" now  
    signE kp keyless

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
    

