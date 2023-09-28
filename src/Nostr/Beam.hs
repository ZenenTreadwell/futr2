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
import Database.Beam as B
import Database.Beam.Sqlite
import Database.SQLite.Simple as SQL
import Database.Beam.Migrate.Simple
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Data.Text (Text)
import Data.Text as T
import Data.List as L 
import Data.Maybe
import Data.Text.Encoding 
import qualified Data.ByteString as BS 
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Data.Aeson
import Data.Int
import Nostr.Db
import Data.Maybe
import Control.Monad.State
import Control.Monad.STM
import Control.Exception
import Control.Concurrent.STM.TChan
import Database.SQLite.Simple.Function as SQL

createDb :: SQL.Connection -> IO (TChan Event) 
createDb o = do 
    runBeamSqlite o $ do 
        veri <- verifySchema migrationBackend spec
        _ <- checkSchema migrationBackend spec mempty
        case veri of 
            VerificationFailed _ -> autoMigrate migrationBackend spec
            VerificationSucceeded -> pure () 
    _ <- execute_ o "CREATE TRIGGER IF NOT EXISTS updaterhook AFTER INSERT ON events BEGIN SELECT eventfeed(NEW.con); END;"
    chan <- newTChanIO
    _ <- createFunction o "eventfeed" (eventfeed chan) 
    pure chan
    
eventfeed :: TChan Event -> Text -> IO Text  
eventfeed chan t = do 
    case qw t of 
        Just e -> do 
            print . content . con $ e
            atomically $ writeTChan chan e
        _ -> pure () 
    pure "1" 

insertPl :: Connection -> Event -> IO () 
insertPl conn e@(Event i _ (Content{..})) = 
    runBeamSqlite conn 
        $ runUpdate
        $ save (_plebs spec') (Pleb (wq pubkey) (Just $ wq e) ) 

insertEv :: Connection -> Event -> IO (Either SQLError ())
insertEv conn e@(Event i _ (Content{..})) =  
    try runIns
    where
    runIns = 
        runBeamSqlite conn $ do
    
        runInsert $ insertOnConflict (_plebs spec') 
                                     (insertExpressions [Pleb (val_ $ wq pubkey) default_])
                                      anyConflict
                                      onConflictDoNothing
       
        runInsert $ B.insert (_events spec') (insertValues [toEv e])
        
        let (mx, rx) = gather . catMaybes $ flip P.map tags \case
                ETag ie _ marker -> Just . Right $ (ie, marker)
                PTag ip _ -> Just . Left $ ip
                _ -> Nothing 

        runInsert . B.insert (_mentions spec') . insertExpressions $ P.map mention mx
        runInsert . B.insert (_replies spec') . insertExpressions $ P.map (uncurry reply) rx

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
      (fromIntegral . kind . con $ e)
      (wq e)

wq :: ToJSON a => a -> Text 
wq = decodeUtf8 . BS.toStrict . encode 

qw :: FromJSON a => Text -> Maybe a
qw = decode . BS.fromStrict . encodeUtf8 

insertId :: Connection -> Text -> IO ()
insertId conn privKey = runBeamSqlite conn $
    runInsert $ B.insert (_identities spec') 
              $ insertValues [Id privKey]

insertRelay :: Connection -> Text -> IO ()
insertRelay db uri = runBeamSqlite db $ do
    runInsert $ B.insert (_relays spec') 
              $ insertExpressions 
              [ Relay default_  (val_ uri) (val_ False) ]

isHex32 :: Text -> Bool 
isHex32 h = case decode . encode $ h of  
    (Just (Hex32 _)) -> True
    _ -> False

toHex32 :: Text -> Maybe Hex32
toHex32 = decode . encode 

fetch :: SQL.Connection -> Filter -> IO [Event]

fetch db (Filter (Just (Ids tx@(P.all isHex32 -> True))) _ _ _ _ _ _ _ )
    = do 
        hx <- mapM (lookupEid db) $ mapMaybe toHex32 tx
        pure . mapMaybe (qw . _con) . catMaybes $ hx
        
fetch db Filter{..} =  
    mapMaybe (qw . _con) <$> runBeamSqlite db (s' d') 
    where 
    s' = case limitF of 
       Just (Limit (fromIntegral -> x)) 
            -> runSelectReturningList . select . nub_ . limit_ x  
       _ -> runSelectReturningList . select . nub_ . limit_ 10000000   
        
    d' = do 
        e <- all_ (_events spec')

        case idsF of 
            Just (Ids (P.map (val_ . ("\""<>) . (<> "%")) -> px)) -> 
                guard_ $ P.foldr ((||.) . like_ (_eid e)) 
                                 (val_ False) px 
            _ -> pure () 
        
        case authorsF of 
            Just (Authors (P.map (val_ . ("\"" <>) . (<> "%")) -> px)) -> 
                guard_ $ P.foldr ((||.) . like_ ((\(PlebId p) -> p) $ _pub e)) 
                                 (val_ False) px
            _ -> pure () 
        
        case kindsF of 
            Just (Kinds (P.map fromIntegral -> kx)) -> 
                guard_ (in_ (_kind e) kx)
            _ -> pure () 
        
        case etagF of 
            Just (ETagM (P.map (val_ . wq) -> ex)) -> do  
                ref <- filter_ (\rep -> in_ (_eidrr rep) ex) 
                               (all_ (_replies spec'))
                guard_ (_eidr ref `references_` e)
            _ -> pure ()

        case ptagF of 
            Just (PTagM (P.map (val_ . wq) -> px)) -> do 
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
            
        pure e
  
popularityContest :: SQL.Connection -> IO _
popularityContest db = runBeamSqlite db (s d) 
    where 
    s = runSelectReturningList . select . limit_ 5 . orderBy_ (desc_ . snd) 
    d = do 
        aggregate_ 
            (\p -> (group_ (_pidm p), as_ @Int32 countAll_) ) 
            (all_ . _mentions $ spec')            
            

activityContest :: SQL.Connection -> IO _
activityContest db = runBeamSqlite db $ s d 
    where 
    s = runSelectReturningList . select . limit_ 5 . orderBy_ (desc_ . snd) 
    d = do 
        aggregate_ 
            (\p -> (group_ (_eidrr p), as_ @Int32 countAll_) ) 
            (all_ . _replies $ spec')            


lookupPid :: SQL.Connection -> Text -> IO _ 
lookupPid db t = P.map (content . con) <$> fetch db emptyF{authorsF=Just aye}
    where 
    aye = Authors [t]
    
lookupEid :: SQL.Connection -> Hex32 -> IO _ 
lookupEid db t = 
    let exi = wq t 
    in 
    runBeamSqlite db 
    $ runSelectReturningOne 
    $ lookup_ (_events spec') (EvId exi)
