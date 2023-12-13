module Futr.Imgs where 

import Network.HTTP.Req 
import Codec.Picture
import Data.Text
import Text.URI
import Monomer
import Data.Vector.Storable.ByteString
import Data.Typeable

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
