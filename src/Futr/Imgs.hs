module Futr.Imgs where 

import Futr.App
import Network.HTTP.Req 
import Codec.Picture
import Data.Text
import Text.URI
import Data.ByteString
import Monomer
import GHC.Float (int2Double)

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
        
showImg :: Text -> Image PixelRGBA8 -> AppNode 
showImg l r = imageMem_ l 
                        (toStrict $ encodeBitmap r) 
                        (Size (int2Double $ imageWidth r) 
                              (int2Double $ imageHeight r))  --[fitWidth] 
                        [fitWidth]
