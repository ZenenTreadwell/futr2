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
import Database.Beam.Backend.SQL

type MyConstraint f be s =
        (Database be Db
        
        , Columnar f Text ~ QGenExpr QValueContext be s Text
        , f ~ QGenExpr QValueContext be s
        -- , Sql92SanityCheck 
        , BeamSqlBackend be
        , HasSqlEqualityCheck be Text

        , BeamSqlBackendIsString be Text

        , HasSqlValueSyntax
                          (Sql92ExpressionValueSyntax
                             (Sql92UpdateExpressionSyntax
                                (Sql92UpdateSyntax (BeamSqlBackendSyntax be))))
                          Text
        
        , Sql92ExpressionFieldNameSyntax
                          (Sql92UpdateExpressionSyntax
                             (Sql92UpdateSyntax (BeamSqlBackendSyntax be)))
                        ~ Sql92UpdateFieldNameSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))
        , Sql92InsertValuesSelectSyntax
                          (Sql92InsertValuesSyntax
                             (Sql92InsertSyntax (BeamSqlBackendSyntax be)))
                        ~ Sql92SelectSyntax (BeamSqlBackendSyntax be)

        , Sql92ExpressionSelectSyntax
                          (Sql92UpdateExpressionSyntax
                             (Sql92UpdateSyntax (BeamSqlBackendSyntax be)))
                        ~ Sql92SelectSyntax (BeamSqlBackendSyntax be)
            
        , Sql92DeleteExpressionSyntax
                          (Sql92DeleteSyntax (BeamSqlBackendSyntax be))
                        ~ Sql92UpdateExpressionSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))

        , Sql92OrderingExpressionSyntax
                          (Sql92SelectOrderingSyntax
                             (Sql92SelectSyntax (BeamSqlBackendSyntax be)))
                        ~ Sql92UpdateExpressionSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))

        , Sql92InsertValuesExpressionSyntax
                          (Sql92InsertValuesSyntax
                             (Sql92InsertSyntax (BeamSqlBackendSyntax be)))
                        ~ Sql92UpdateExpressionSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))

        , Sql92TableSourceExpressionSyntax
                          (Sql92FromTableSourceSyntax
                             (Sql92SelectTableFromSyntax
                                (Sql92SelectSelectTableSyntax
                                   (Sql92SelectSyntax (BeamSqlBackendSyntax be)))))
                        ~ Sql92UpdateExpressionSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))

        , Sql92ProjectionExpressionSyntax
                          (Sql92SelectTableProjectionSyntax
                             (Sql92SelectSelectTableSyntax
                                (Sql92SelectSyntax (BeamSqlBackendSyntax be))))
                        ~ Sql92UpdateExpressionSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))

        , Sql92FromExpressionSyntax
                          (Sql92SelectTableFromSyntax
                             (Sql92SelectSelectTableSyntax
                                (Sql92SelectSyntax (BeamSqlBackendSyntax be))))
                        ~ Sql92UpdateExpressionSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))

        , Sql92TableSourceSelectSyntax
                          (Sql92FromTableSourceSyntax
                             (Sql92SelectTableFromSyntax
                                (Sql92SelectSelectTableSyntax
                                   (Sql92SelectSyntax (BeamSqlBackendSyntax be)))))
                        ~ Sql92SelectSyntax (BeamSqlBackendSyntax be)


        , Sql92SelectSelectTableSyntax
                          (Sql92SelectTableSelectSyntax
                             (Sql92SelectSelectTableSyntax
                                (Sql92SelectSyntax (BeamSqlBackendSyntax be))))
                        ~ Sql92SelectSelectTableSyntax
                            (Sql92SelectSyntax (BeamSqlBackendSyntax be))

        , Sql92GroupingExpressionSyntax
                          (Sql92SelectTableGroupingSyntax
                             (Sql92SelectSelectTableSyntax
                                (Sql92SelectSyntax (BeamSqlBackendSyntax be))))
                        ~ Sql92UpdateExpressionSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))

        , Sql92SelectTableExpressionSyntax
                          (Sql92SelectSelectTableSyntax
                             (Sql92SelectSyntax (BeamSqlBackendSyntax be)))
                        ~ Sql92UpdateExpressionSyntax
                            (Sql92UpdateSyntax (BeamSqlBackendSyntax be))
        )


class Qable a where 
    qf :: MyConstraint f be s => EvT f -> a -> Q be Db s () -- (EvT (QExpr be s))

--           EvT f -> a -> Q be Db s () -- (EvT (QExpr be s))
--           -- QGenExpr context be s Bool

instance Qable Ids where 
    qf e (Ids px) = guard_ 
                  . foldr anym (val_ False) 
                  . map (val_ . (<> "%")) 
                  $ px 
        where 
        anym = (||.) . like_ (_eid e)

instance Qable Authors where 
    qf e (Authors px) = guard_
                      . foldr anym (val_ False) 
                      . map (val_ . (<> "%"))
                      $ px
        where 
        anym = (||.) . like_ ((\(PlebId p) -> p) $ _pub e)


-- XXX
instance Qable Kinds where 
    qf e (Kinds kx) = guard_
                    . in_ (_kind e)
                    . map fromIntegral
                    $ kx
                    
-- XXX 

instance Qable ETagM where 
    qf e (ETagM (map (val_ . wq) -> ex)) = do  
        ref <- filter_ (\rep -> in_ (_eidrr rep) ex) 
                       (all_ (_replies spec'))
        guard_ (_eidr ref `references_` e)

  
getQf :: -- MyConstraint f be s => 
      Filter -> Q Sqlite Db s (EvT (QExpr Sqlite s))
getQf fi@Filter{} = do  
    e <- all_ (_events spec')
    guard_ $ fromMaybe_ currentTimestamp_ (_expires e) >=. currentTimestamp_ 
    fullQf fi e
    pure e

                          
                        
fullQf (Filter {..}) e = do
    mguard idsF
    mguard authorsF       

    -- XXX converting this to mguard broke
    mguard kindsF      
    -- case kindsF of 
    --         Just (Kinds (map fromIntegral -> kx)) -> 
    --             guard_ (in_ (_kind e) kx)
    --         _ -> pure () 
            -- XXX
        
    -- XXX drawing board    
    mguard etagF 
    -- case etagF of 
    --     Just (ETagM (map (val_ . wq) -> ex)) -> do  
    --         ref <- filter_ (\rep -> in_ (_eidrr rep) ex) 
    --                        (all_ (_replies spec'))
    --         guard_ (_eidr ref `references_` e)
    --     _ -> pure ()
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
        
    where 
    mguard :: Qable s => Maybe s -> _ 
    mguard = maybe (pure ()) (qf e)      
            
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

fetchx :: Connection -> [Filter] -> IO [Event]
fetchx db fx = nub . mconcat <$> mapM (fetch db) fx 

fetch :: Connection -> Filter -> IO [Event]

fetch _ (Filter _ _ _ _ _ _ _ _ (Just (Limit 0))) = return []

fetch db (Filter (Just (Ids tx@(all isHex32 -> True))) _ _ _ _ _ _ _ _ ) = do 
    hx <- mapM (lookupEid db) $ mapMaybe toHex32 tx
    pure . mapMaybe (qw . _con) . catMaybes $ hx
        
fetch db ff@Filter{..} =  
    mapMaybe (qw . _con) <$> runBeamSqlite db (sf d) 
    where 
    -- s' :: _ -- Q Sqlite db x y 
    sf = case limitF of 
       Just (Limit (fromIntegral -> x)) 
            -> runSelectReturningList . select 
                                      . limit_ x 
                                      . nub_ 
                                      . orderBy_ (desc_ . _time) 

       _ -> runSelectReturningList . select 
                                   . limit_ 10000000 
                                   . nub_ 
                                   . orderBy_ (desc_ . _time) 
        
    d = getQf ff  
