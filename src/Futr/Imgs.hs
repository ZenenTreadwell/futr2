module Futr.Imgs where 

import Network.HTTP.Req 
import Codec.Picture
import Data.Text
import Text.URI
import Monomer
import Data.Vector.Storable.ByteString
import Data.Typeable
import Diagrams.Core
import Diagrams.Backend.Rasterific
import Diagrams.Example.Logo
import Diagrams.TwoD

-- data Options Rasterific V2 n = RasterificOptions
--        { _size      :: SizeSpec2D n -- ^ The requested size of the output
--        }

-- type family Result Rasterific V2 n = Image PixelRGBA8

-- renderDia :: Rasterific -> Options Rasterific V2 n -> QDiagram Rasterific V2 n m -> 'Image PixelRGBA8'

-- data Options Rasterific V2 n = RasterificOptions
--     { _size :: SizeSpec2D n -- ^ The requested size of the output
--     }
-- type instance Result Rasterific V2 n = Image PixelRGBA8

drawImg :: (Typeable a, Typeable b) => WidgetNode a b
drawImg = showImg "diagrams" $ r -- Rasterific (RasterificOptions (mkWidth 250)) logo
    where 
    -- r :: Rasterific -> Options Rasterific V2 n -> QDiagram Rasterific V2 n m -> Image PixelRGBA8
    -- r :: Result Rasterific V2 (Image PixelRGBA8)
    -- r = renderDia Rasterific (RasterificOptions (mkWidth (fromIntegral (250::Int)))) logo
    r :: Result Rasterific V2 (Image PixelRGBA8)
    r = renderDia Rasterific (RasterificOptions (mkWidth ((250 :: Double)))) logo

fetchImg :: URI -> IO (Image PixelRGBA8) -- ByteString
fetchImg uri = 
    let d :: Url a -> Req BsResponse
        d u = req GET u NoReqBody bsResponse mempty 
    in do 
    (responseBody -> bs) <- runReq defaultHttpConfig $  
        case useURI uri of 
            Just (Right (fst -> r)) -> d r
            Just (Left (fst -> l)) -> d l
            Nothing -> undefined
            
    pure case decodeImage bs of 
        Left l -> error l 
        Right r -> convertRGBA8 r 
        
showImg :: (Typeable a , Typeable b) => Text -> Image PixelRGBA8 -> WidgetNode a b 
showImg l r = imageMem_ l 
                        (vectorToByteString . imageData $ r) 
                        (Size (fromIntegral $ imageWidth r) 
                              (fromIntegral $ imageHeight r))  --[fitWidth] 
                        [fitWidth]
