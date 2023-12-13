module Futr.App where 

import Prelude as P
import Monomer as O 
import Nostr.Event as N
import Nostr.Pool
import Data.Text as T
import Text.URI
import qualified Data.Map as M 
import Data.Sequence
import Codec.Picture

type AppNode = WidgetNode AppModel AppEvent
type AppEnv = WidgetEnv AppModel AppEvent
data AppModel = AppModel {
        theme :: Theme 
      -- , msgs :: [Event] -- Graph
      , imgs :: Seq (URI, Image PixelRGBA8)
      -- , imgl :: M.Map URI 
      --                 (Image PixelRGBA8) 
      , pool :: Pool'
      , texts :: Text
    } deriving (Eq)

data AppEvent = 
      AppInit
    | Nada
    | ReModel AppModel
    | TextField Text
    -- | FreshPool Pool'
    -- | SwitchTheme Theme
    -- | A [Event]
    -- | NextImg (Maybe Int)
    -- | LoadImg (URI , Image PixelRGBA8) 
