
module Futr.Gui where 
import Futr.Imgs

import Prelude as P
import Monomer as O 
import qualified Data.ByteString as B
import Data.ByteString.Lazy as LB
import Nostr.Event as N
import Nostr.Pool
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Nostr.Kinds
import Database.SQLite.Simple as SQL
import Control.Monad
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Data.Text as T
import Data.Maybe
import Text.URI
import qualified Data.Map as M 
import Network.HTTP.Req 
import Codec.Picture
import GHC.Float
import Control.Concurrent.Async
import Control.Monad.State
import Data.Time.Clock
import Data.Sequence as S
import Data.Foldable
import Data.Typeable
        
getImgs :: Kind -> [URI]
getImgs (Kind1 u _ _) = u
getImgs _ = []

data TTTModel = TTTModel Text deriving Eq
data TTTEvent = 
      ChangeTTT Text 
    | ReportTTT

buildTTT :: UIBuilder TTTModel TTTEvent
buildTTT  _ (TTTModel t) = textFieldV t ChangeTTT

handlTTT :: (Typeable a, Typeable b) => EventHandler TTTModel TTTEvent a b  
handlTTT _ _ _ (ChangeTTT t) = [Model . TTTModel $ t]
handlTTT _ _ _ _ = []

compositeTTT 
    :: (Typeable a, Typeable b)  => TTTModel 
    -> (TTTModel -> TTTEvent) 
    -> UIBuilder TTTModel TTTEvent
    -> EventHandler TTTModel TTTEvent a b
    -> WidgetNode a b

compositeTTT = compositeV (WidgetType "ttt") 

tttextfield :: (Typeable a, Typeable b) => WidgetNode a b
tttextfield = compositeTTT (TTTModel "past") (const ReportTTT) buildTTT handlTTT

pandoras :: 
    (Typeable a, Typeable b) => 
    WidgetNode a b -> WidgetNode a b
pandoras = box_ [mergeRequired (\_ _ _ -> False)]  
     
isIs :: Tag -> Maybe Text 
isIs (AZTag 't' x) = Just x
isIs _ = Nothing  
    
labelconfig :: (Typeable a, Typeable b) =>  [LabelCfg a b]
labelconfig = [O.multiline, trimSpaces]
