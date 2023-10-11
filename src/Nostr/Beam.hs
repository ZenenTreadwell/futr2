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
import Numeric
import Database.Beam as B
import Database.Beam.Sqlite
import Database.SQLite.Simple as SQL
import Database.Beam.Migrate.Simple
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Data.Text (Text)
import Data.Time
import Data.Time.Clock
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
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Char8 as BS8
import Control.Concurrent
import Data.Time.Clock.POSIX

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
        Just e -> atomically $ writeTChan chan e
        _ -> pure () 
    pure "1" 

insertPl :: Connection -> Event -> IO () 
insertPl conn e@(Event i _ (Content{..})) = 
    runBeamSqlite conn 
        $ runUpdate
        $ save (_plebs spec') (Pleb (wq pubkey) (Just $ wq e) ) 

insertDm :: Connection -> Event -> IO () 
insertDm conn e = runBeamSqlite conn $ runInsert . B.insert (_dms spec') 
    $ insertExpressions 
    [ Dm default_ (val_ . BS.toStrict . encode $ e) (val_ False) (val_ . PlebId . wq $ p)]
    where 
    p = case L.filter isPTag . tags . con $ e of 
            (PTag x _) : _ -> x 
            _ -> error "invalid dm" 
    isPTag (PTag{}) = True
    isPTag _ = False    

insertEv :: Connection -> Event -> IO (Either SQLError ())
insertEv conn e@(Event i _ (Content{..})) = do 
    ex <- calcExpiry e
    try . runBeamSqlite conn $ do
        runInsert $ insertOnConflict (_plebs spec') 
                                     (insertExpressions [Pleb (val_ $ wq pubkey) default_])
                                     anyConflict
                                     onConflictDoNothing
        runInsert $ B.insert (_events spec') (insertValues [toEv ex e])
        mapM_ insertTz tags 
    where 
    insertTz :: Tag -> SqliteM ()
    insertTz = \case  
        ETag ie _ marker -> into (_replies spec') [reply ie marker] 
        PTag ip _ -> into (_mentions spec') [mention ip] 
        AZTag c t -> 
            let azid :: ByteString
                azid = SHA256.hash  . BS.toStrict  . encode  $ (c,t)
            in do 
            runInsert $ insertOnConflict (_azs spec') 
                (insertExpressions [Az (val_ azid)])
                anyConflict
                onConflictDoNothing 

            into (_azt spec') [AzRef default_ 
                                   (val_ . AzId $ azid) 
                                   (val_ . EvId . wq $ i)
                              ] 

        _ -> pure ()  
        
    reply :: Hex32 -> Maybe Marker -> ReplyT (QExpr Sqlite m) 
    reply id' marker = 
        Reply default_ (val_ . EvId . wq $ i) (val_ . wq $ id') (val_ marker)
    
    mention :: Hex32 -> MentionT (QExpr Sqlite m) 
    mention id' = Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id') 

into b = runInsert . B.insert b . insertExpressions

toEv :: Maybe LocalTime -> Event -> EvT Identity 
toEv x e = Ev 
      (wq $ eid e) 
      (PlebId . wq . pubkey . con $ e) 
      (fromInteger . created_at . con $ e) 
      (fromIntegral . kind . con $ e)
      x
      (wq e)

calcExpiry :: Event -> IO (Maybe LocalTime)
calcExpiry e = case L.filter isExp . tags . con $ e of 
    (Expiry x) : _ -> pure . Just 
                           . zonedTimeToLocalTime 
                           . utcToZonedTime utc 
                           . posixSecondsToUTCTime 
                           . realToFrac $ x
    _ -> case kind . con $ e of 
        (isEphemeral -> True) -> do 
            zo <- getCurrentTimeZone
            ti <- addUTCTime fifteen <$> getCurrentTime
            let ow = zonedTimeToLocalTime $ utcToZonedTime zo ti
            pure . Just $ ow
        _ -> pure Nothing
    where 
    isExp (Expiry{}) = True
    isExp _ = False  
    isEphemeral k = k >= 10000 && k < 20000
    fifteen :: NominalDiffTime
    fifteen = secondsToNominalDiffTime . realToFrac $ 15 * 60 

wq :: ToJSON a => a -> Text 
wq = decodeUtf8 . BS.toStrict . encode 

qw :: FromJSON a => Text -> Maybe a
qw = decode . BS.fromStrict . encodeUtf8 

insertId :: Connection -> ByteString -> IO ()
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

fetch _ (Filter _ _ _ _ _ _ _ _ (Just (Limit 0))) = return []

fetch db (Filter (Just (Ids tx@(P.all isHex32 -> True))) _ _ _ _ _ _ _ _ ) = do 
    hx <- mapM (lookupEid db) $ mapMaybe toHex32 tx
    pure . mapMaybe (qw . _con) . catMaybes $ hx
        
fetch db ff@Filter{..} =  
    mapMaybe (qw . _con) <$> runBeamSqlite db (s' d') 
    where 
    -- s' :: _ -- Q Sqlite db x y 
    s' = case limitF of 
       Just (Limit (fromIntegral -> x)) 
            -> runSelectReturningList . select . nub_ . limit_ x  
       _ -> runSelectReturningList . select . nub_ . limit_ 10000000   
        
    d' = getQf ff  
  
getQf :: Filter -> Q Sqlite Db s (EvT (QExpr Sqlite s))
getQf Filter{..} = 
    do  e <- all_ (_events spec')
    

        -- guard_ $ _expires e <. currentTimestamp_ 
        
        case idsF of 
            Just (Ids (P.map (val_ . ("\""<>). (<>"%")) -> px)) -> 
                guard_ $ P.foldr ((||.) . like_ (_eid e)) 
                                 (val_ False) px 
            _ -> pure () 
        
        case authorsF of 
            Just (Authors (P.map (val_ . ("\""<>). (<> "%")) -> px)) -> 
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

        flip mapM_ aztagF \case 
            AZTag c t -> do  
                let azid = SHA256.hash  . BS.toStrict  . encode  $ (c,t)
                ref <- filter_ (\rf -> (val_ . AzId $ azid) ==. (_azref rf)) (all_ (_azt spec'))
                guard_ (_iieid ref `references_` e)
            _ -> pure ()
            
        pure e

countFx :: SQL.Connection -> Filter -> IO [Int32]
countFx db ff = runBeamSqlite db 
    $ runSelectReturningList . select 
    $ aggregate_ (\_ -> as_ @Int32 countAll_) 
    $ nub_ 
    $ getQf ff


popularityContest :: SQL.Connection -> IO _
popularityContest db = runBeamSqlite db (s d) 
    where 
    -- s :: _
    s = runSelectReturningList . select . limit_ 5 . orderBy_ (desc_ . snd) 
    -- d :: _
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
    runBeamSqlite db 
    $ runSelectReturningOne 
    $ lookup_ (_events spec') (EvId . wq $ t)
