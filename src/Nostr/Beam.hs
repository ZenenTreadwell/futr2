{-# LANGUAGE
      DeriveAnyClass 
    , StandaloneDeriving
    , TypeFamilies
    , UndecidableInstances 
    , PartialTypeSignatures
    , ImpredicativeTypes
    , DuplicateRecordFields
    #-}

module Nostr.Beam where

import Prelude as P
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple as SQL
import Database.Beam.Migrate.Simple
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Data.Text (Text)
import Data.Text as T
import Data.Maybe
import Data.Text.Encoding 
import qualified Data.ByteString as BS 
import Nostr.Event
import Nostr.Filter
import Data.Aeson
import Nostr.Db
import Control.Monad.State
import Control.Exception

createDb :: SQL.Connection -> IO () 
createDb conn = runBeamSqlite conn $ do 
   veri <- verifySchema migrationBackend spec
   _ <- checkSchema migrationBackend spec mempty
   case veri of 
       VerificationFailed _ -> autoMigrate migrationBackend spec
       VerificationSucceeded -> pure () 


insertPl :: Connection -> Event -> IO () 
insertPl conn e@(Event i _ (Content{..})) = 
    runBeamSqlite conn 
        $ runUpdate
        $ save (_plebs spec') (Pleb (wq pubkey) (Just $ wq e) ) 

insertEv :: Connection -> Event -> IO ()
insertEv conn e@(Event i _ (Content{..})) =  
    catch runIns \(e :: SQLError)-> do 
        print "SQLERROR!!!!"
        print e 
    where
    runIns = 
        -- withTransaction conn $ 
        -- runBeamSqliteDebug print conn $ do
        runBeamSqlite conn $ do
    
        runInsert $ insertOnConflict (_plebs spec') 
                                     (insertExpressions [Pleb (val_ $ wq pubkey) default_])
                                      anyConflict
                                      onConflictDoNothing
       
                                                                                               
        runInsert $ insert (_events spec') (insertValues [toEv e])
        let (mx, rx) = gather . catMaybes $ flip P.map tags \case
                ETag ie _ marker -> Just . Right $ (ie, marker)
                PTag ip _ -> Just . Left $ ip
                _ -> Nothing 

        
        runInsert . insert (_mentions spec') . insertExpressions $ P.map mention mx
        runInsert . insert (_replies spec') . insertExpressions $ P.map (uncurry reply) rx

    gather :: [Either a b] ->  ([a], [b]) 
    gather = P.foldr g ([],[]) 
        where 
        g (Right r) (mx, rx) = (mx, r:rx)  
        g (Left l) (mx, rx) = (l:mx, rx)
    
    reply :: Hex32 -> Maybe Marker -> ReplyT (QExpr Sqlite m) 
    reply id' marker = 
        Reply default_ (val_ . EvId . wq $ i) (val_ . wq $ id') (val_ marker)
    
    mention :: Hex32 -> MentionT (QExpr Sqlite m) 
    mention id' = Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id') 

toEv :: Event -> EvT Identity 
toEv e = Ev 
      (wq $ eid e) 
      (PlebId . wq . pubkey . con $ e) 
      (fromInteger . created_at . con $ e) 
      (wq e)

wq :: ToJSON a => a -> Text 
wq = decodeUtf8 . BS.toStrict . encode 

qw :: FromJSON a => Text -> Maybe a
qw = decode . BS.fromStrict . encodeUtf8 

insertId :: Connection -> Text -> IO ()
insertId conn privKey = runBeamSqlite conn $
    runInsert $ insert (_identities spec') 
              $ insertValues [Id privKey]

insertRelay :: Connection -> Text -> IO ()
insertRelay db uri = runBeamSqlite db $ do
    runInsert $ insert (_relays spec') 
              $ insertExpressions 
              [ Relay default_  (val_ uri) (val_ False) ]


fetchBaseline :: SQL.Connection -> Filter -> IO [Event]
fetchBaseline db f@Filter{..} = do 
    P.filter (flip matchF f) . catMaybes . P.map (qw . _con)  
        <$> runBeamSqlite db (s d) 
    where 
    s = runSelectReturningList . select 
    d = all_ (_events spec')
       

fetch :: SQL.Connection -> Filter -> IO [Event]
fetch db Filter{..} = do 
    catMaybes . P.map (qw . _con)  
        <$> runBeamSqlite db (s d) 
    where 
    -- s :: _ 
    s = case limitF of 
       Just (Limit (fromIntegral -> x)) -> runSelectReturningList . select . limit_ x  
       -- type error if limit not included
       _ -> runSelectReturningList . select . limit_ 10000000   
        
    d = do 
        e <- all_ (_events spec')
        case idsF of 
            Just (Ids (P.map (val_ . (<> "%")) -> px)) -> 
                guard_ $ P.foldr (||.) (val_ False) 
                       $ P.map (like_ (_eid e)) px
            _ -> pure () 
        
        case authorsF of 
            Just (Authors (P.map (val_ . (<> "%")) -> px)) -> 
                guard_ $ P.foldr (||.) (val_ False) 
                       $ P.map (like_ ((\(PlebId p) -> p) $ _pub e)) px
            _ -> pure () 
        
        -- case kindF c -- only support kind 1 anyway?
        
        case etagF of 
            Just (ETagM (P.map (val_ . wq) -> ex)) -> do  
                ref <- filter_ (\rep ->
                  (in_ (_eidrr rep) ex)) (all_ (_replies spec'))
                guard_ (_eidr ref `references_` e)
            _ -> pure ()

        case ptagF of 
            Just (PTagM (P.map (val_ .wq) -> px)) -> do 
                ref <- filter_ (\men -> (
                    (in_ (_pidm men) px)
                    )) (all_ $ _mentions spec')
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
            
        pure e
  
