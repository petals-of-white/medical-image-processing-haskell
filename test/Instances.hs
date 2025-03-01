{-# OPTIONS_GHC -Wno-orphans #-}
module Instances where
import qualified Data.Vector.Storable         as VS
import           Foreign.Storable             (Storable)
import           Graphics.Gloss.Data.Extent
import           Graphics.Gloss.Data.QuadTree
import           Segmentation
import           System.Random
import           Test.QuickCheck              as QuickCheck
import           Vision.Image
import           Vision.Primitive


maxImageSize :: Int
maxImageSize = 30

instance (Arbitrary p, Storable p) => Arbitrary (Manifest p) where
    arbitrary = sized $ \n -> do
        w <- choose (1, n)
        h <- choose (1, n)
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

instance Arbitrary a => Arbitrary (QuadTree a) where
    arbitrary = sized $ \n -> do
            depth <- choose (0, n)
            if n == 0 then return TNil
            else
                let smaller = QuickCheck.resize (n - 1) arbitrary in
                frequency [(1, return TNil), (1, TLeaf <$> arbitrary), (depth, TNode <$> smaller <*> smaller <*> smaller <*> smaller)]

instance (Arbitrary a, Integral a,  Random a) => Arbitrary (RegionStatistics a) where
    arbitrary = sized $ \n -> do
        min :: a <- arbitrary
        max :: a <- choose (min, min + fromIntegral n)
        avg <- choose (min, max)
        numPix <- arbitrary
        let sumI = fromIntegral avg * numPix
        stdD <- choose (0, abs (max - min))
        return RegionStatistics {
            regionMin = min,
            regionMax = max,
            regionAverage = avg,
            regionStd = stdD,
            regionSumIntensity = sumI,
            regionNumberOfPixels = numPix
        }

instance Arbitrary Extent where
    arbitrary = sized $ \n -> do
        south <- nonNeg
        west <- nonNeg
        north <- choose (south, south + n)
        east <- choose (west, west + n)
        return $ makeExtent north south east west
        where nonNeg = getNonNegative <$> arbitrary
