import           Codec.Picture
import           Data.Dicom
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import           Graphics.Gloss.Data.Extent (makeExtent)
import           QuadTree
import           Segmentation
import           Utilities
import           Vision.Image as I
import           Vision.Image.JuicyPixels   (toFridayGrey)
import           Vision.Primitive.Shape
import System.Environment (getArgs)
import DICOM
import Criterion.Main
import Data.Word (Word16)
import Data.Binary (Word8)
import Histogram (windowLevel)
import qualified Data.Vector.Storable as VS
import Control.DeepSeq
import GHC.Generics

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
    -- mapM_ print (take 7 elements)
    let quadImage = quadTreeToImage' quadTree imgSize
        splitName = "split_" ++ show stdThreshByte ++ "_" ++ show avgDiffThreshByte ++ ".bmp"
        (GreyPixel stdThreshByte) = stdThresh
        (GreyPixel avgDiffThreshByte) = avgDiffThresh
        mergeName = "merge_" ++ show stdThreshByte ++ "_" ++ show avgDiffThreshByte ++ ".bmp"
    saveBMP "greyscale.bmp" img

    saveBMP splitName quadImage

    let merged = mergeRegions1 avgDiffThresh quadTree (makeExtent h 0 w 0)
        mergedManifest = groupRegionsToManifest merged imgSize

        sameIdGroups   =
                    [   (gr1, gr2) |
                        gr1 <- merged, gr2 <- merged,
                        groupId gr1 == groupId gr2,
                        let childSet1 = Set.fromList $ NonEmpty.toList (childrenRegions gr1),
                        let childSet2 = Set.fromList $ NonEmpty.toList (childrenRegions gr2),
                        not (childSet1 `Set.isSubsetOf` childSet2 ||  childSet2 `Set.isSubsetOf` childSet1)]



    -- putStrLn (unlines (showGroup <$> merged))

    case sameIdGroups of
        [] -> putStrLn "okay"
        _ -> do
            putStrLn "error, some groups are crazy"
            putStrLn   $ unlines $ take 10 $  Prelude.map (\(g1, g2) -> unlines [showGroup g1, showGroup g2]) sameIdGroups
    saveBMP mergeName mergedManifest



main:: IO ()
main =
    defaultMain [
        (env readDicomManifest $ \mnfst ->
            bgroup "gaussian blur"  [
                bench "r=1" $ whnf (gaussianBlur 1 (Nothing::Maybe Float) :: Manifest Word16 -> Manifest Word16) mnfst,
                bench "r=2" $ whnf (gaussianBlur 2 (Nothing::Maybe Float) :: Manifest Word16 -> Manifest Word16) mnfst,
                bench "r=3" $ whnf (gaussianBlur 3 (Nothing::Maybe Float) :: Manifest Word16 -> Manifest Word16) mnfst,
                bench "r=4" $ whnf (gaussianBlur 3 (Nothing::Maybe Float) :: Manifest Word16 -> Manifest Word16) mnfst,
                bench "r=5" $ whnf (gaussianBlur 5  (Nothing::Maybe Float) :: Manifest Word16 -> Manifest Word16) mnfst,
                bench "r=10" $ whnf (gaussianBlur 10 (Nothing::Maybe Float) :: Manifest Word16 -> Manifest Word16) mnfst
            ]
        ),
        (env (readDicomFile "brain_001.dcm") $ \bytestr ->
            bench "dicom parsing" $ nf parseDicomFile bytestr
        )


    ]
applyBlur :: Manifest Word16 -> Manifest Word16
applyBlur = gaussianBlur 2 (Nothing :: Maybe Float)

readDicomManifest :: IO (Manifest Word16)
readDicomManifest = do
    dcmBS <- readDicomFile "brain_001.dcm"
    let (Right dicomObj) = parseDicomFile dcmBS
        (Right mnfs) = manifestFromDICOM dicomObj
    return mnfs

readDCM :: IO (Either String [DicomElement])
readDCM = do
    dcmBS <- readDicomFile "brain_001.dcm"
    return $ parseDicomFile dcmBS

-- instance Convertible Word16 RGBPixel where
--   safeConvert = _
convertProportionally :: forall a b. (Bounded a, Real a, Bounded b, Real b, Convertible Float b) => a -> b
convertProportionally fromV = minB + convert (float (fromV - minA) /  float (maxBound - minA) * realToFrac (maxBound - minB))
    where   minA = minBound :: a
            minB = minBound :: b
            float = realToFrac :: a -> Float


-- instance Generic Tag
-- instance NFData Tag

-- instance Generic VR
-- instance NFData VR

deriving instance Generic Tag
instance NFData Tag

deriving instance Generic VR
instance NFData VR

deriving instance Generic DicomElement
-- instance Generic DicomElement
instance NFData DicomElement
tryBlur :: Int -> IO ()
tryBlur radius  = do
    image <- readDicomManifest
    let blurred@Manifest {manifestVector=blurV} = gaussianBlur radius (Nothing :: Maybe Float) image :: Manifest Word16
        normalized@Manifest{manifestVector=normV} = windowLevel 440 880 minBound maxBound blurred

    putStrLn $ "Maximum is " ++ show (VS.maximum normV)
    putStrLn $ "Min is " ++ show (VS.minimum normV)
    saveBMP "gauss.bmp" (I.map (GreyPixel . convertProportionally) normalized)


    -- case manifestFromDICOM dicomObj of
    --     Right mnfst -> 
    --         defaul [
    --             bench "gauss blur: r=1" $ whnf (gaussianBlur 1 (Nothing :: Maybe Float) :: Manifest Word16 -> Manifest Word16) mnfst
    --         ]
    --     Left err -> putStrLn ("Error:" ++ err)
-- main:: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         path:_ -> do
--             dcmBS <- readDicomFile path
--             let dicomParseResult = parseDicomFile dcmBS
--             case dicomParseResult of
--                 Right dicomObj ->
--                     case manifestFromDICOM dicomObj of
--                         Right mnfst -> 
--                             defaul [
--                                 bench "gauss blur: r=1" $ whnf (gaussianBlur 1 (Nothing :: Maybe Float) :: Manifest Word16 -> Manifest Word16) mnfst
--                             ]
--                         Left err -> putStrLn ("Error:" ++ err)
--                 Left err -> putStrLn ("Error: " ++ err)
--         [] -> putStrLn "Usage: script <dicom_path>"
