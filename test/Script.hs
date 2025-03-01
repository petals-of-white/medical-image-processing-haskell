module Script where
import Segmentation
import Vision.Image
import Codec.Picture
import Vision.Image.JuicyPixels (toFridayGrey)
import Data.Maybe (fromJust)
import QuadTree 
import Vision.Primitive.Shape
import Graphics.Gloss.Data.Extent (makeExtent)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty
import Utilities 

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

testSplitMerge :: FilePath -> GreyPixel -> GreyPixel -> IO ()
testSplitMerge path stdThresh avgDiffThresh  = do
    img <- loadGrayscale path

    let quadTree = regionSplit ((<= stdThresh).  regionStd . makeStats) makeStats img
        makeStats = fromJust . computeStats
        imgSize@(Z :. h :. w) = manifestSize img

        elements = dfsQuadTreeWithExt quadTree (makeExtent h 0 w 0)

    putStrLn $ "image loaded with size " ++ show imgSize

    putStrLn $ "There are " ++ show (length elements) ++ " segments"
    mapM_ print (take 7 elements) 
    let quadImage = quadTreeToImage' quadTree imgSize

    saveBMP "red.bmp" img

    saveBMP "split.bmp" quadImage

    let merged = mergeRegions1 avgDiffThresh quadTree (makeExtent h 0 w 0)
        mergedManifest = groupRegionsToManifest merged imgSize

        sameIdGroups   = 
                    [   (gr1, gr2) | 
                        gr1 <- merged, gr2 <- merged,
                        groupId gr1 == groupId gr2,
                        let childSet1 = Set.fromList $ NonEmpty.toList (childrenRegions gr1),
                        let childSet2 = Set.fromList $ NonEmpty.toList (childrenRegions gr2),
                        not (childSet1 `Set.isSubsetOf` childSet2 ||  childSet2 `Set.isSubsetOf` childSet1)]
        


    putStrLn (unlines (showGroup <$> merged))   

    case sameIdGroups of
        [] -> putStrLn "okay"
        _ -> do 
            putStrLn "error, some groups are crazy"
            putStrLn   $ unlines $ take 10 $  Prelude.map (\(g1, g2) -> unlines [showGroup g1, showGroup g2]) sameIdGroups
    saveBMP "merged.bmp" mergedManifest


main:: IO ()
main = putStrLn "Hello, Haskell!"
