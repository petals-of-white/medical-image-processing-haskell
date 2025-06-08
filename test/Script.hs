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

loadGrayscale :: FilePath -> IO (Manifest Int)
loadGrayscale path = do
    imageRes <- readImage path
    return $
        toFridayGrey $
        case imageRes of
            Left err -> error err
            Right img -> case img of
                ImageRGBA8 rgba -> pixelMap (\(PixelRGBA8 r g b _) -> fromIntegral (blendRGB' r g b)) rgba
                ImageRGB8 rgb -> fromIntegral $ pixelMap blendRGB rgb
                ImageY8 y -> pixelMap fromIntegral y
                ImageY16 y16 -> pixelMap fromIntegral y16
                ImageYA8 img -> pixelMap (\(PixelYA8 y _) -> fromIntegral y) img
                ImageYA16 img -> pixelMap (\(PixelYA16 y _) -> fromIntegral y) img
                ImageRGB16 rgb -> pixelMap (\(PixelRGB16 r g b) -> fromIntegral (blendRGB' r g b)) rgb
                -- ImageRGBF rgb -> pixelMap (\(PixelRGBF r g b) -> truncate $ (blendRGBf r g b) * (maxBound :: Int)) rgb

                other -> error $ "Cannot convert image to grayscale:" ++ show other


instance Show DynamicImage where
    show dImg = case dImg of
        ImageY8 _ -> "ImageY"
        ImageY16 _ -> "ImageY16"
        ImageYA8 _ -> "ImageYA8"
        ImageYA16 _ -> "ImageYA16"
        ImageRGB8 _ -> "ImageRGB8"
        ImageRGB16 _ -> "ImageRGB16"
        ImageRGBA8 _ -> "ImageRGBA8"
        ImageRGBA16 _ -> "ImageRGBA16"
        ImageRGBF _ -> "ImageRGBF"
        ImageYF _ -> "ImageYF"


blendRGB' :: (Integral a) => a -> a -> a -> a
blendRGB' r g b = round $ 0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b

blendRGBf :: (Fractional a) => a -> a -> a -> a
blendRGBf r g b = 0.299 * r + 0.587 * g + 0.114 * b

blendRGB :: PixelRGB8 -> Pixel8
blendRGB (PixelRGB8 r g b) = round $ 0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b


testSplitMerge :: FilePath -> Int -> Int -> IO ()
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
        splitName = "split_" ++ show stdThresh ++ "_" ++ show avgDiffThresh ++ ".bmp"
        -- (GreyPixel stdThreshByte) = stdThresh
        -- (GreyPixel avgDiffThreshByte) = avgDiffThresh
        mergeName = "merge_" ++ show stdThresh ++ "_" ++ show avgDiffThresh ++ ".bmp"

    saveBMP "greyscale.bmp" $ I.map (GreyPixel . convertProportionally) img

    saveBMP splitName $ I.map (GreyPixel . convertProportionally) quadImage

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
    saveBMP mergeName $ I.map (GreyPixel . convertProportionally) mergedManifest



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
