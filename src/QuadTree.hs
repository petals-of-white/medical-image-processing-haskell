{-# OPTIONS_GHC -Wno-orphans #-}
module QuadTree where
import           Foreign.Storable             (Storable)
import           Graphics.Gloss.Data.Extent
import           Graphics.Gloss.Data.Quad
import           Graphics.Gloss.Data.QuadTree
import           Vision.Image
import           Vision.Primitive
import Data.Maybe (fromMaybe)
import           System.IO.Unsafe                      (unsafePerformIO)


instance Functor QuadTree where
    fmap f (TLeaf a) = TLeaf (f a)
    fmap f (TNode nw ne sw se) = TNode (fmap f nw) (fmap f ne) (fmap f sw) (fmap f se)
    fmap _ TNil = TNil


-- quadTreeToImage :: (Storable pix, Num pix) => 
--     (a -> Extent -> pix) -- ^ Function to create pixel from node value and extent
--     -> QuadTree a -- ^ QuadTree
--     -> Size -- ^ Image size
--     -> Manifest pix

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

-- groupRegionsToManifest :: VS.Storable a => [GroupRegion a] -> Size -> Manifest a
-- groupRegionsToManifest groupRegions imgSize = unsafePerformIO $ do
--     mutManifest :: MutableManifest c a <- new imgSize
--     mapM_
--         (\groupRegion ->
--             mapM_
--                 (writeExt mutManifest (regionAverage $ groupRegionStats groupRegion) . regionExtent)
--                 (childrenRegions groupRegion)
--         )
--         groupRegions

--     unsafeFreeze mutManifest

--     where writeExt mutImg value ext =
--             let (n, s, e, w) = takeExtent ext
--                 points = [(y, x) | y <- [s..n], x <- [w..e]]
            -- in mapM_ (\(y, x) -> write mutImg (ix2 (n - y) x) value) points

dfsQuadTree :: QuadTree a -> [a]
dfsQuadTree TNil = []
dfsQuadTree (TLeaf a) = [a]
dfsQuadTree (TNode nw ne sw se) = concat [dfsQuadTree nw, dfsQuadTree ne, dfsQuadTree sw, dfsQuadTree se]


dfsQuadTreeWithExt :: QuadTree a -> Extent -> [(a, Extent)]
dfsQuadTreeWithExt TNil _ = []
dfsQuadTreeWithExt (TLeaf a) extent = [(a, extent)]
dfsQuadTreeWithExt (TNode nw ne sw se) extent =
    let extNE = cutQuadOfExtent NE extent
        extNW = cutQuadOfExtent NW extent
        extSE = cutQuadOfExtent SE extent
        extSW = cutQuadOfExtent SW extent
    in  concat [dfsQuadTreeWithExt nw extNW, dfsQuadTreeWithExt ne extNE, dfsQuadTreeWithExt se extSE, dfsQuadTreeWithExt sw extSW]

