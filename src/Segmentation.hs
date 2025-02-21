{-# OPTIONS_GHC -Wno-orphans #-}
module Segmentation where

import Graphics.Gloss.Data.QuadTree
import Vision.Image as I
import Vision.Primitive
import qualified Data.Vector.Storable as VS

import qualified Control.Foldl as Foldl
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Data.Quad
import Data.Maybe
import Vision.Image.JuicyPixels
import Codec.Picture.Bitmap (writeBitmap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Convertible (convert)
instance Functor QuadTree where
    fmap f (TLeaf a) = TLeaf (f a)
    fmap f (TNode nw ne sw se) = TNode (fmap f nw) (fmap f ne) (fmap f sw) (fmap f se)
    fmap _ TNil = TNil


-- windowLevel :: Real a => a -> a -> a -> a -> Manifest a -> Manifest a
-- windowLevel wCenter wWidth newMax newMin img = I.map f img where
--     f pixel | pixel < minThresh = newMin
--             | pixel > maxThresh = newMax
--             | otherwise = newMin + (newMax - newMin) * (pixel - minThresh) / wWidth
--     minThresh = wCenter - wWidth / 2
--     maxThresh = wCenter + wWidth / 2 


windowLevel :: (Real a, Ord a, Convertible Float a, VS.Storable a) => a -> a -> a -> a -> Manifest a -> Manifest a
windowLevel wCenter wWidth newMax newMin img = I.map f img where
    f pixel | float pixel < minThresh = newMin
            | float pixel > maxThresh = newMax
            | otherwise = newMin + convert ((float (newMax - newMin) * (float pixel - minThresh)) / float wWidth)
    minThresh = float wCenter - float wWidth / 2
    maxThresh = float wCenter + float wWidth / 2 
    float x = realToFrac x :: Float


isHomogeneous :: (Real a, VS.Storable a) => Manifest a -> Bool
isHomogeneous img = Foldl.fold Foldl.std (Prelude.map realToFrac $ VS.toList $ manifestVector img) <= 10

areHomogeneous :: Manifest Int -> Manifest Int -> Bool
areHomogeneous = undefined

treeToImage :: QuadTree Int -> Size -> Manifest Int
treeToImage quadTree size@(Z :. h :. w) = fromFunction size $ fromMaybe 0 . getPixel quadTree (makeExtent h 0 w 0) . swapY
    where
        getPixel (TLeaf n) extent coord = if coordInExtent extent coord then Just n else Nothing
        getPixel (TNode nw ne sw se) extent coord =
            case quadOfCoord extent coord of
                Just pos ->
                    case pos of
                        NW -> getPixel nw (cutQuadOfExtent pos extent) coord
                        NE -> getPixel ne (cutQuadOfExtent pos extent) coord
                        SE -> getPixel se (cutQuadOfExtent pos extent) coord
                        SW -> getPixel sw (cutQuadOfExtent pos extent) coord
                Nothing -> Nothing
        getPixel TNil _ _ = Nothing

        swapY (Z :. y :. x) = (x, (h - y))


saveBMP :: VS.Storable a => FilePath -> (a -> RGBPixel) -> Manifest a -> IO ()
saveBMP path f img =
    saveRGB path (I.map f img)

saveRGB :: FilePath -> Manifest RGBPixel -> IO ()
saveRGB path img =
    writeBitmap path $ toJuicyRGB img

-- mergeTwo ::
--     QuadTree (Point, Size, Int) ->
--     QuadTree (Point, Size, Int)
--     -> (QuadTree (Point, Size, Int), QuadTree (Point, Size, Int))

-- mergeTwo (TNode) = T

-- regionMerge :: QuadTree (Point, Size, Int) -> QuadTree (Point, Size, Int)
-- regionMerge = undefined

-- data Region = Region {
--     regionId :: Int,
--     regionExtent :: Extent
-- }

-- data RAG region = RAG {
--     nodes :: Map.Map Int region,
--     edges :: Map.Map Int (Set.Set Int)
-- }


-- mergeRegions :: forall el region. QuadTree el -> Extent -> (el -> Extent -> region) -> (region -> region -> region) -> (region -> Bool) -> ()


regionSplit :: (Real a, VS.Storable a) => Manifest a -> QuadTree (Point, Size, Int)
regionSplit img = fst $ makeTree (ix2 0 0) imgSize 0
    where
        imgSize@(Z :. imgHeight :. imgWidth) = manifestSize img
        makeTree :: Point -> Size -> Int -> (QuadTree (Point, Size, Int), Int)
        makeTree point@(Z :. y :. x) size@(Z :. h :. w) currentSegment
            | y < 0 || x < 0 || y + h  > imgHeight || x + w > imgWidth = error "Invalid region"
            | h == 1 || w == 1 = (TLeaf (point, size, currentSegment), currentSegment + 1)
            | otherwise =
                let region = crop (Rect x y w h) img
                    halfH = ceiling (fromIntegral h / 2 :: Double)
                    halfW = ceiling (fromIntegral w / 2 :: Double)
                    (nw, segmentN1) = makeTree point (ix2 halfH halfW) currentSegment
                    (ne, segmentN2) = makeTree (ix2 y (x + halfW)) (ix2 halfH  (w - halfW)) segmentN1
                    (sw, segmentN3) = makeTree (ix2 (y + halfH) x) (ix2 (h - halfH) halfW) segmentN2
                    (se, segmentN4) = makeTree (ix2 halfH halfW) (ix2 (h - halfH) (w - halfW)) segmentN3
                in
                    if isHomogeneous region
                    then (TLeaf (point, size, currentSegment), currentSegment + 1)
                    else (TNode nw ne sw se, segmentN4)

            -- | h == 1 && w == 1 = TLeaf (point, size, prevSegment + 1)
            -- | otherwise =

            --             TNode topLeft topRight bottomLeft bottomRight
