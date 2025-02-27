module Script where
import Segmentation
import Vision.Image
import Codec.Picture
import Vision.Image.JuicyPixels (toFridayGrey)
import Data.Maybe (fromJust)
import QuadTree 
import Vision.Primitive.Shape
import Graphics.Gloss.Data.Extent (makeExtent)



loadGrayscale :: FilePath -> IO (Manifest GreyPixel)
loadGrayscale path = do
    imageRes <- readImage path
    return $
        toFridayGrey $
        case imageRes of
            Left err -> error err
            Right img -> case img of
                ImageRGBA8 rgba -> pixelMap (\(PixelRGBA8 r _ _ _) -> r) rgba
                ImageRGB8 rgb -> pixelMap blendRGB rgb
                ImageRGBF rgb -> pixelMap (\(PixelRGBF r _ _) -> truncate $ r * 255) rgb
                ImageY8 y -> y
                _ -> error "Cannot convert image to grayscale"

blendRGB :: PixelRGB8 -> Pixel8
blendRGB (PixelRGB8 r g b) = round $ 0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b

testSplitMerge :: FilePath -> GreyPixel -> IO ()
testSplitMerge path thresh = do
    img <- loadGrayscale path

    let quadTree = regionSplit ((<= thresh).  regionStd . makeStats) makeStats img
        makeStats = fromJust . computeStats
        imgSize@(Z :. h :. w) = manifestSize img

        elements = dfsQuadTreeWithExt quadTree (makeExtent h 0 w 0)

    putStrLn $ "image loaded with size " ++ show imgSize

    putStrLn $ "There are " ++ show (length elements) ++ " segments"
    mapM_ print (take 7 elements) 
    let quadImage = quadTreeToImage (\(RegionStatistics{regionAverage=avg}, _) _ -> avg) quadTree imgSize

    saveBMP "red.bmp" img
    -- print quadTree
    saveBMP "split.bmp" quadImage

    let merged = mergeRegions mkRegion (\(value, _sId) ext group -> tryMergeByDiffTreshold 8 (Region value ext) group) quadTree (makeExtent h 0 w 0)
        mergedManifest = groupRegionsToManifest merged imgSize    
    saveBMP "merged.bmp" mergedManifest

    where mkRegion (stat, _id) ext = fromSingleRegion (Region stat ext)
main:: IO ()
main = putStrLn "Hello, Haskell!"
