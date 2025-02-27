{-# OPTIONS_GHC -Wno-orphans #-}
import Test.QuickCheck
import Segmentation
import QuadTree
import Vision.Image
import Foreign.Storable (Storable)
import qualified Data.Vector.Storable as VS
import Vision.Primitive
import Data.Set as Set
import Data.Maybe (fromJust)
import Graphics.Gloss.Data.Extent (makeExtent)
import Data.Word (Word16)
import Data.List (nub)

prop_splitMakesAllPixelsSegmented ::  Manifest Word16 -> Property
prop_splitMakesAllPixelsSegmented img@Manifest{manifestSize = Z :. height :. width} =
    counterexample ("Failed at number of segments :" ++ show (length segments) ++ ". " ++ show segments) $
    Set.fromList (coordsInExtent imgExtent) === Set.fromList (coordsInQuadTree segmented imgExtent)
    where   imgExtent = makeExtent height 0 width 0
            segmented = Segmentation.regionSplit ((<=  thresh) . regionStd . makeStats) makeStats img
            thresh = 12
            makeStats = fromJust . Segmentation.computeStats
            segments = snd <$> dfsQuadTreeWithExt segmented imgExtent


prop_noPixelOverlap :: Manifest Word16 -> Property
prop_noPixelOverlap img@Manifest{manifestSize = Z :. height :. width} =
    counterexample ("Failed at number of segments :" ++ show (length segments) ++ ". " ++ show segments) $
    nub coveredPixels === coveredPixels
    where   imgExtent = makeExtent height 0 width 0
            segmented = Segmentation.regionSplit ((<=  thresh) . regionStd . makeStats) makeStats img
            segments = snd <$> dfsQuadTreeWithExt segmented imgExtent
            thresh = 12
            makeStats = fromJust . Segmentation.computeStats
            coveredPixels = coordsInQuadTree segmented imgExtent

maxImageSize :: Int
maxImageSize = 30

instance (Arbitrary p, Storable p) => Arbitrary (Manifest p) where
    arbitrary = do
        w <- choose (1, maxImageSize)
        h <- choose (1, maxImageSize)
        vec  <- VS.replicateM (w * h) arbitrary
        return $ Manifest (Z :. h :. w) vec
    shrink img@Manifest {manifestSize=Z:.h:.w}
        | w * h <= 1 = []
        | otherwise =
            let halfWidth = ceiling (fromIntegral w / 2 :: Float)
                halfHeight = ceiling (fromIntegral h / 2 :: Float)
                nw = crop (Rect 0 0 halfWidth halfHeight) img
                ne = crop (Rect halfWidth 0 (w - halfWidth) halfHeight) img
                sw = crop (Rect 0 halfHeight halfWidth (h - halfHeight)) img
                se = crop (Rect halfWidth halfHeight (w - halfWidth) (h - halfHeight)) img
            in [nw, ne, sw , se]

main :: IO ()
main = do
    putStrLn "All pixels are segmented?"
    quickCheck prop_splitMakesAllPixelsSegmented

    putStrLn "No overlapping?"
    quickCheck prop_noPixelOverlap
