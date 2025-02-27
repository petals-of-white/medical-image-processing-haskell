{-# OPTIONS_GHC -Wno-orphans #-}
module QuadTree where
import           Data.Maybe                   (fromMaybe)
import           Foreign.Storable             (Storable)
import           Graphics.Gloss.Data.Extent
import           Graphics.Gloss.Data.Quad
import           Graphics.Gloss.Data.QuadTree
import           System.IO.Unsafe             (unsafePerformIO)
import           Vision.Image
import           Vision.Primitive

coordsInExtent :: Extent -> [Coord]
coordsInExtent ext = [(x,y) | x <- [s..n-1], y <- [w..e-1]]
    where (n,s,e,w) = takeExtent ext

coordsInQuadTree :: QuadTree a -> Extent -> [Coord]
coordsInQuadTree quadTree extent = concat [coordsInExtent ext | (_, ext) <- regions]
    where regions = dfsQuadTreeWithExt quadTree extent

instance Functor QuadTree where
    fmap f (TLeaf a) = TLeaf (f a)
    fmap f (TNode nw ne sw se) = TNode (fmap f nw) (fmap f ne) (fmap f sw) (fmap f se)
    fmap _ TNil = TNil
-- quadTreeToImage makePixel quadTree size@(Z :. h :. w) = fromFunction size $ fromMaybe 0 . getPixel quadTree (makeExtent h 0 w 0) . swapY
--     where
--         getPixel (TLeaf n) extent coord = if coordInExtent extent coord then Just (makePixel n extent) else Nothing
--         getPixel (TNode nw ne sw se) extent coord =
--             case quadOfCoord extent coord of
--                 Just pos ->
--                     case pos of
--                         NW -> getPixel nw (cutQuadOfExtent pos extent) coord
--                         NE -> getPixel ne (cutQuadOfExtent pos extent) coord
--                         SE -> getPixel se (cutQuadOfExtent pos extent) coord
--                         SW -> getPixel sw (cutQuadOfExtent pos extent) coord
--                 Nothing -> Nothing
--         getPixel TNil _ _ = Nothing

--         swapY (Z :. y :. x) = (x, h - y)

quadTreeToImage :: (Storable pix, Show pix, Num pix) =>
    (a -> Extent -> pix) -- ^ Function to create pixel from node value and extent
    -> QuadTree a -- ^ QuadTree
    -> Size -- ^ Image size
    -> Manifest pix

quadTreeToImage makePixel quadTree size@(Z :. height :. width) = unsafePerformIO $ do
    mutImg :: MutableManifest c a <- new' size 255
    let dfsList = dfsQuadTreeWithExt quadTree (makeExtent height 0 width 0)
    -- print $ Prelude.map snd dfsList

    mapM_ (\(v, ext) -> writeExt mutImg (makePixel v ext) ext) dfsList
    freeze mutImg

    where writeExt mutImg value ext =
            let (n, s, e, west) = takeExtent ext
                points = [(y, x) | y <- [s..n-1], x <- [west..e-1]]
            in mapM_ (\(y, x) -> do
                -- putStrLn $ "Writing " ++ show value ++ " to " ++ show (y, x)
                write mutImg (ix2 (height - 1 - y) x) value) points

dfsQuadTree :: QuadTree a -> [a]
dfsQuadTree TNil = []
dfsQuadTree (TLeaf a) = [a]
dfsQuadTree (TNode nw ne sw se) = concat [dfsQuadTree nw, dfsQuadTree ne, dfsQuadTree sw, dfsQuadTree se]



-- (nw, segmentN1) = makeTree (ix2 y x) (ix2 halfH halfW) currentSegment
-- (ne, segmentN2) = makeTree (ix2 y (x + halfW)) (ix2 halfH (w - halfW)) segmentN1
-- (sw, segmentN3) = makeTree (ix2 (y + halfH) x) (ix2 (h - halfH) halfW) segmentN2
-- (se, segmentN4) = makeTree (ix2 (y + halfH) (x + halfW)) (ix2 (h - halfH) (w - halfW)) segmentN3

extentToRect :: Extent -> Rect
extentToRect extent = Rect {rX = w, rY = s, rWidth = e - w, rHeight = n - s}
    where (n, s, e, w) = takeExtent extent


rectToExtent :: Rect -> Extent
rectToExtent Rect {rX, rY, rWidth, rHeight} = makeExtent (rY + rHeight) rY (rX + rWidth) rX


takeQuadOfExtent :: Quad -> Extent -> Extent
takeQuadOfExtent quad extent =
    case quad of
        NW -> makeExtent n (n - halfHeight) (w + halfWidth)  w
        NE -> makeExtent n (n - halfHeight) e (w + halfWidth)
        SW -> makeExtent (n - halfHeight) s (w + halfWidth)  w
        SE -> makeExtent (n - halfHeight) s e (w + halfWidth)
    where
        (n, s, e, w) = takeExtent extent
        halfHeight = ceiling $ fromIntegral (n - s) / (2 :: Float)
        halfWidth = ceiling $ fromIntegral (e - w) / (2 :: Float)


dfsQuadTreeWithExt :: QuadTree a -> Extent -> [(a, Extent)]
dfsQuadTreeWithExt TNil _ = []
dfsQuadTreeWithExt (TLeaf a) extent = [(a, extent)]
dfsQuadTreeWithExt (TNode nw ne sw se) extent =
    let extNE = takeQuadOfExtent NE extent
        extNW = takeQuadOfExtent NW extent
        extSE = takeQuadOfExtent SE extent
        extSW = takeQuadOfExtent SW extent
    in  concat [dfsQuadTreeWithExt nw extNW, dfsQuadTreeWithExt ne extNE, dfsQuadTreeWithExt se extSE, dfsQuadTreeWithExt sw extSW]

