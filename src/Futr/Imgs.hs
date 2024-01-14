module Futr.Imgs where 

import Network.HTTP.Req 
import Codec.Picture
import Data.Text (Text)
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



-- side = sqrt 2
-- triangleRect :: Diagram B
-- triangleRect = polygon ( with
--   & polyType .~ PolySides
--      [ 135 @@ deg, 90 @@ deg]
--      [ 1        , side      ]
--   )

-- triangleLeft = triangleRect # rotateBy (1/2) # fc white # lc white # lw none
-- triangleRight = triangleRect # fc black #lc black # lw none

-- smallTile = beside (r2 (1,-1)) (triangleLeft # align (r2 (1, -1)))
--                                 triangleRight
-- smallTile' :: Int -> Diagram B
-- smallTile' x = smallTile # rotate x'
--   where x' = fromIntegral x *pi/2 @@ rad

-- createMatrix x = matrix # alignX 0 # alignY 0
--   where matrix = (x !! 0 ||| x !! 1 )
--                          ===
--                  (x !! 2 ||| x !! 3)
-- mediumTile angles = createMatrix (map smallTile' angles)
-- largeTile :: [Int] -> Bool -> Bool -> Diagram B
-- largeTile angles xSymmetry ySymmetry = createMatrix [a, b, c, d]
--   where
--     a = mediumTile $ chunks !! 0
--     b = if ySymmetry then a # reflectX else mediumTile $ chunks !! 1
--     c = if xSymmetry then a # reflectY else mediumTile $ chunks !! 2
--     d
--       | ySymmetry && xSymmetry = a # rotateBy (-1/2)
--       | ySymmetry  = c # reflectX
--       | xSymmetry  = b # reflectY
--       | otherwise = mediumTile $ chunks !! 3
--     chunks = chunksOf 4 angles
-- centerPos x = (x-0.5)*4 + (x-1)*d
--     where d = 1.5
-- randInts :: Int -> [Int]
-- randInts seed = randomRs (0, 3) (mkStdGen seed)
-- example :: Diagram B
-- example = position (zip (map p2 pos) (zipWith (curry largeTile') angles nbAxes))
--   where
--     nb = 10
--     pos = [(centerPos x, centerPos y) | x <- [1..nb], y <- [1..nb]]
--     angles = take (nb*nb) $ chunksOf 16 $ randInts 15
--     nbAxes = take (nb*nb) $ randInts 12
-- main = mainWith (example :: Diagram B)
-- -- Needs a list of 16 angles and the number of axes
-- largeTile' :: ([Int], Int) -> Diagram B
-- largeTile' x = largeTile n xSymmetry ySymmetry
--   where
--     n = fst x
--     nbAxes = snd x
--     xSymmetry = nbAxes == 1 || nbAxes == 3
--     ySymmetry = nbAxes == 2 || nbAxes == 3
    
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
