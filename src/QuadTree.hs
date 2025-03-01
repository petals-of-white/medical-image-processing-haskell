{-# OPTIONS_GHC -Wno-orphans #-}
module QuadTree where
import           Data.Vector.Storable.Mutable (PrimMonad, PrimState)
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

quadTreeToImage :: (Storable pix) =>
    (a -> Extent -> pix) -- ^ Function to create pixel from node value and extent
    -> QuadTree a -- ^ QuadTree
    -> Size -- ^ Image size
    -> Manifest pix

quadTreeToImage makePixel quadTree size@(Z :. height :. width) = unsafePerformIO $ do
    mutImg :: MutableManifest c a <- new size
    let dfsList = dfsQuadTreeWithExt quadTree (makeExtent height 0 width 0)
    -- print $ Prelude.map snd dfsList

    mapM_ (\(v, ext) -> writeRect mutImg (makePixel v ext) (extentToRect ext height)) dfsList
    freeze mutImg


dfsQuadTree :: QuadTree a -> [a]
dfsQuadTree TNil = []
dfsQuadTree (TLeaf a) = [a]
dfsQuadTree (TNode nw ne sw se) = concat [dfsQuadTree nw, dfsQuadTree ne, dfsQuadTree sw, dfsQuadTree se]

extentToRect :: Extent -> Int -> Rect
extentToRect extent height = Rect {rX = w, rY = height - n, rWidth = e - w, rHeight = n - s}
    where (n, s, e, w) = takeExtent extent

writeRect :: (Storable p, PrimMonad m) => MutableManifest p (PrimState m) -> p -> Rect -> m ()
writeRect mutImg value Rect {rX, rY, rWidth, rHeight} =
    let points = [(y, x) | y <- [rY .. rY + rHeight - 1], x <- [rX .. rX + rWidth - 1]]
    in  mapM_ (\(y, x) -> write mutImg (ix2 y x) value) points

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

