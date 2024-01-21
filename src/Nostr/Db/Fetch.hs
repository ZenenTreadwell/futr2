module Nostr.Db.Fetch where 

import Nostr.Filter
import Nostr.Db.Schema
import Nostr.Db.Insert
import Nostr.Event
import Nostr.Keys


import Data.Int
import Data.Maybe 
import Data.Text (Text)
import Data.List (nub)
import Data.Aeson

import Control.Monad

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple 

fetchx :: Connection -> [Filter] -> IO [Event]
fetchx db fx = nub . mconcat <$> mapM (fetch db) fx 

fetch :: Connection -> Filter -> IO [Event]

fetch _ (Filter _ _ _ _ _ _ _ _ (Just (Limit 0))) = return []

fetch db (Filter (Just (Ids tx@(all isHex32 -> True))) _ _ _ _ _ _ _ _ ) = do 
    hx <- mapM (lookupEid db) $ mapMaybe toHex32 tx
    pure . mapMaybe (qw . _con) . catMaybes $ hx
        
fetch db ff@Filter{..} =  
    mapMaybe (qw . _con) <$> runBeamSqlite db (s' d') 
    where 
    -- s' :: _ -- Q Sqlite db x y 
    s' = case limitF of 
       Just (Limit (fromIntegral -> x)) 
            -> runSelectReturningList . select 
                                      . limit_ x 
                                      . nub_ 
                                      . orderBy_ (desc_ . _time) 

       _ -> runSelectReturningList . select 
                                   . limit_ 10000000 
                                   . nub_ 
                                   . orderBy_ (desc_ . _time) 
        
    d' = getQf ff  
  
getQf :: Filter -> Q Sqlite Db s (EvT (QExpr Sqlite s))
getQf fi@Filter{} = do  
    e <- all_ (_events spec')
    guard_ $ fromMaybe_ currentTimestamp_ (_expires e) >=. currentTimestamp_ 
    fullQf fi e
    pure e


idsQf :: EvT (QGenExpr QValueContext Sqlite s) -> Maybe Ids -> Q Sqlite Db s ()
idsQf e (Just (Ids px)) =  guard_ . foldr ((||.) . like_ (_eid e)) (val_ False) 
                                  . map (val_ . (<>"%")) 
                                  $ px 
idsQf _ _ = pure () 

                            
fullQf :: Filter -> EvT (QGenExpr QValueContext Sqlite s) -> Q Sqlite Db s () --
fullQf (Filter {..}) e  = do
    idsQf e idsF    
    case authorsF of 
        Just (Authors (map (val_ . (<> "%")) -> px)) -> 
            guard_ $ foldr ((||.) . like_ ((\(PlebId p) -> p) $ _pub e)) 
                             (val_ False) px
        _ -> pure () 
    
    case kindsF of 
        Just (Kinds (map fromIntegral -> kx)) -> 
            guard_ (in_ (_kind e) kx)
        _ -> pure () 
    
    case etagF of 
        Just (ETagM (map (val_ . wq) -> ex)) -> do  
            ref <- filter_ (\rep -> in_ (_eidrr rep) ex) 
                           (all_ (_replies spec'))
            guard_ (_eidr ref `references_` e)
        _ -> pure ()
    case ptagF of 
        Just (PTagM (map (val_ . wq) -> px)) -> do 
            ref <- filter_ (\men -> in_ (_pidm men) px)
                           (all_ $ _mentions spec')
            guard_ (_eidm ref `references_` e)
        _ -> pure () 
    case sinceF of 
        Just (Since (fromIntegral -> s)) -> 
            guard_ $ (_time e >. val_ s )
        _ -> pure ()
    case untilF of 
        Just (Until (fromIntegral -> u)) -> 
            guard_ $ (_time e <. val_ u )
        _ -> pure () 
    forM_ aztagF \case 
        az -> do  
            ref <- filter_ (\rf -> val_ (hashtag az) ==. _azref rf) (all_ (_azt spec'))
            guard_ (_iieid ref `references_` e)
        _ -> pure ()
        
            
countFx :: Connection -> Filter -> IO [Int32]
countFx db ff = runBeamSqlite db 
    $ runSelectReturningList . select 
    $ aggregate_ (\_ -> as_ @Int32 countAll_) 
    $ nub_ 
    $ getQf ff
    
isHex32 :: Text -> Bool 
isHex32 h = case decode . encode $ h of  
    (Just (Hex32 _)) -> True
    _ -> False

lookupEid :: Connection -> Hex32 -> IO (Maybe (Ev Identity)) 
lookupEid db t = 
    runBeamSqlite db 
    $ runSelectReturningOne 
    $ lookup_ (_events spec') (EvId . wq $ t)


toHex32 :: Text -> Maybe Hex32
toHex32 = decode . encode 
