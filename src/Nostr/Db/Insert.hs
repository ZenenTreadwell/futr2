module Nostr.Db.Insert where

import Nostr.Event 
import Nostr.Keys
import Nostr.Db.Schema
import Nostr.Kinds

import Database.SQLite.Simple
import Database.Beam.Sqlite
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Data.Foldable

import Control.Exception
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Monad 

type WriteDb = TChan (IO ())

type WriteReadDb = (WriteDb, Connection)

insertLoop :: WriteDb -> IO () 
insertLoop tc = forever do 
    ins <- atomically (readTChan tc) 
    ins

data InsMode = Regular | Replace | ParReplace | Delete deriving (Show)

insMode :: Event -> InsMode 
insMode e = case kind . con $ e of
    0 -> Replace 
    5 -> Delete
    ((<10000) -> True) -> Regular
    ((<20000) -> True) -> Replace
    ((<30000) -> True) -> Regular 
    ((<40000) -> True) -> ParReplace 
    _ -> Regular

insertEv :: Connection -> Event -> IO (Either SQLError ())
insertEv conn e@(Event i _ (Content{..})) = do 
    ex <- calcExpiry e
    try . runBeamSqlite conn $ do
        runInsert $ insertOnConflict (_plebs spec') 
                                     (insertExpressions [Pleb (val_ $ wq pubkey)])
                                     anyConflict
                                     onConflictDoNothing
        case insMode e of 
            Regular -> pure ()  
            Delete -> forM_ tags \case 
                ETag (wq -> ee) _ _ -> retireByEid pubkey ee
                ATag (ARef k _ md) _ -> case md of 
                    Nothing -> runUpdate $ update (_events spec')
                        (\e' -> _expires e' <-. val_ (mxpiry 1337) ) 
                        (\e' ->     _kind e' ==. val_ (fromIntegral k)
                                &&. (PlebId (val_ $ wq pubkey) ==. _pub e')
                        )
                    Just d -> do 
                        me <- replLook k pubkey (AZTag 'd' d)  
                        for_ me (retireByEid pubkey)
                _ -> pure () 
            Replace -> do 
                e' :: Maybe Text <- runSelectReturningOne $ select do
                    ee <- all_ (_events spec')
                    guard_ $ _pub ee ==. (val_ . PlebId . wq $ pubkey)
                    guard_ $ _kind ee ==. (val_ . (fromIntegral :: Int -> Int32) $ kind)
                    pure . _eid $ ee
                for_ e' removeEv 
            ParReplace -> do 
                let mfirstD = find isD $ tags  
                e' <- case mfirstD of 
                    Just z -> replLook kind pubkey z
                    _ -> pure Nothing 
                for_ e' removeEv
        runInsert $ insert (_events spec') (insertValues [toEv ex e])
        mapM_ insertTz $ tags <> getContentTags e 
    where 
    isD :: Tag -> Bool
    isD (AZTag 'd' _) = True
    isD _ = False
    insertTz :: Tag -> SqliteM () 
    insertTz = \case  
        ETag ie _ marker -> runInsert ( 
            (insert (_replies spec')) (insertExpressions [reply ie marker]) 
            )
        PTag ip _ _ -> runInsert $ (insert (_mentions spec')) (insertExpressions [mention ip]) 
        az@(AZTag _ _) -> runInsert $ (insert (_azt spec')) (insertExpressions [AzRef default_ 
                                       (val_ (hashtag az)) 
                                       (val_ . EvId . wq $ i)
                                  ]) 

        _ -> pure ()  
        
    
    reply :: Hex32 -> Maybe Marker -> ReplyT (QExpr Sqlite m) 
    reply id' marker = 
        Reply default_ (val_ . EvId . wq $ i) (val_ . wq $ id') (val_ marker)
    
    mention :: Hex32 -> MentionT (QExpr Sqlite m) 
    mention id' = Mention default_ (val_ . EvId . wq $ i) (val_ . wq $ id') 

    
        
    
calcExpiry :: Event -> IO (Maybe LocalTime)
calcExpiry e = case filter isExp . tags . con $ e of 
    (Expiry x) : _ -> pure . mxpiry $ x 
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
    isEphemeral k = k >= 20000 && k < 30000

mxpiry :: Int64 -> Maybe LocalTime
mxpiry = Just . zonedTimeToLocalTime  . utcToZonedTime utc 
              . posixSecondsToUTCTime . realToFrac 
            
removeEv :: Text -> SqliteM ()
removeEv ee' = do 
    runDelete $ delete (_azt spec') 
                (\a -> (val_ (EvId ee') ==.) . _iieid $ a) 
    runDelete $ delete (_mentions spec')
                (\a -> (val_ (EvId ee') ==.) . _eidm $ a) 
    runDelete $ delete (_replies spec')
                (\a -> (val_ (EvId ee') ==.) . _eidr $ a) 
    runDelete $ delete (_events spec') 
                (\a -> (val_ ee' ==.) . _eid $ a) 
    
fifteen :: NominalDiffTime
fifteen = secondsToNominalDiffTime . realToFrac $ 15 * 60 

replLook :: Int -> Hex32 -> Tag -> SqliteM (Maybe Text) 
replLook kind pubkey az@(AZTag c t) =  
    runSelectReturningOne $ select do
        ee <- all_ (_events spec')
        ref <- filter_ 
                   (\rf ->  val_ (hashtag az) ==. _azref rf) 
                   (all_ (_azt spec'))
        guard_ (_iieid ref `references_` ee)
        guard_ $ _pub ee ==. (val_ . PlebId . wq $ pubkey)
        guard_ $ _kind ee ==. (val_ . (fromIntegral :: Int -> Int32) $ kind)
        pure . _eid $ ee
replLook _ _ _ = pure Nothing 

retireByEid :: Hex32 -> Text -> SqliteM () 
retireByEid pubkey ee = runUpdate $ update (_events spec') 
    (\e' -> _expires e' <-. val_ (mxpiry 1337) ) 
    (\e' -> (&&.)  
            (PlebId (val_ $ wq pubkey) ==. _pub e')
            (val_ ee ==. _eid e')
    )    


toEv :: Maybe LocalTime -> Event -> EvT Identity 
toEv x e = Ev 
      (wq $ eid e) 
      (PlebId . wq . pubkey . con $ e) 
      (fromInteger . created_at . con $ e) 
      (fromIntegral . kind . con $ e)
      x
      (wq e)

    
    
