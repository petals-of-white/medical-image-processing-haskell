
module Segmentation where

import           Codec.Picture.Bitmap         (writeBitmap)
import qualified Control.Foldl                as Foldl
import           Data.List                    as List
import           Data.List.NonEmpty
import qualified Data.Vector.Storable         as VS
import           System.IO.Unsafe                      (unsafePerformIO)
import           Graphics.Gloss.Data.Extent
import           Graphics.Gloss.Data.QuadTree
import           QuadTree
import           Vision.Image                 as I
import           Vision.Image.JuicyPixels
import           Vision.Primitive
import Data.Either (rights)
import Data.Monoid (All(All))

-- | Check if standard deviation of the region is less than threshold
stdThreshold :: (Real a, VS.Storable a) => Double -> Manifest a ->  Bool
stdThreshold thresh img = std <= thresh
    where std = Foldl.fold Foldl.std (Prelude.map realToFrac $ VS.toList $ manifestVector img)

computeStats :: forall a. (Integral a, VS.Storable a) => Manifest a -> Maybe (RegionStatistics a)
computeStats img =
    Foldl.fold folder $
    List.map (fromIntegral :: a -> Double) $
    VS.toList $ manifestVector img
    where
        maybeStats mMin mMax mMean mStd mLen = fmap (fmap truncate) $  RegionStatistics <$> mMin <*> mMax <*> mMean <*> mStd <*> mLen
        folder = 
            maybeStats <$> Foldl.minimum <*> Foldl.maximum 
            <*> fmap Just Foldl.mean <*> fmap Just Foldl.std
            <*> pure (Just $ shapeLength (manifestSize img))
    

data RegionStatistics pix = RegionStatistics {
    regionMin     :: pix,
    regionMax     :: pix,
    regionAverage :: pix,
    regionStd     :: pix,
    regionNumberOfPixels :: Int
    } deriving (Functor, Show)

data Region pix = Region {regionStats :: RegionStatistics pix, regionExtent :: Extent}


data GroupRegion pix = GroupRegion {
    groupRegionStats :: RegionStatistics pix,
    childrenRegions  :: NonEmpty (Region pix)
    }

fromSingleRegion :: Region v -> GroupRegion v
fromSingleRegion region = GroupRegion (regionStats region) (region :| [])

data RegionError = NotHomogeneous | NotConnected

tryMergeByDiffTreshold :: Integral a => a -> Region a -> GroupRegion a -> Either RegionError (GroupRegion a)
tryMergeByDiffTreshold avgDiffTreshold region groupRegion =
    if abs (regionAverage regStats - regionAverage groupStats) <= avgDiffTreshold
    then if region `isTouchingGroup` groupRegion
        then         
            Right GroupRegion {
                groupRegionStats = RegionStatistics {
                    regionMin = min (regionMin regStats) (regionMin groupStats),
                    regionMax = max (regionMax regStats) (regionMax groupStats),
                    regionAverage = round (realToFrac (regionAverage regStats + regionAverage groupStats) / 2),
                    regionStd = regionStd regStats + regionStd groupStats,
                    regionNumberOfPixels = regionNumberOfPixels regStats + regionNumberOfPixels groupStats
                    },
                childrenRegions = region <| childrenRegions groupRegion
            }
        else Left NotConnected
    else Left NotHomogeneous

    where 
        GroupRegion {groupRegionStats = groupStats} = groupRegion
        Region {regionStats=regStats} = region


isTouchingGroup :: Region a -> GroupRegion a -> Bool
isTouchingGroup Region{regionExtent=rExt} GroupRegion {childrenRegions=children} =
    let (All result) = mconcat $ 
            [All (childExt `extentNeighbours` rExt) 
            |  Region{regionExtent=childExt} <- toList children]
    in result


extentNeighbours :: Extent -> Extent -> Bool
extentNeighbours ext1 ext2 =
    let (n1, s1, e1, w1) = takeExtent ext1
        (n2, s2, e2, w2) = takeExtent ext2
    in not (e1 < w2 || e2 < w1 || n1 < s2 || n2 < s1)
-- mergeGroupRegions :: Integral a => a -> Region a -> GroupRegion a -> GroupRegion a
-- mergeGroupRegions avgDiffTreshold (GroupRegion stats1 regions1) (GroupRegion stats2 regions2) =
--     GroupRegion
--         (RegionStatistics {
--             regionMin = min (regionMin stats1) (regionMin stats2),
--             regionMax = max (regionMax stats1) (regionMax stats2),
--             regionAverage = round (realToFrac (regionAverage stats1 + regionAverage stats2) / 2),
--             regionStd = regionStd stats1 + regionStd stats2,
--             regionNumberOfPixels = regionNumberOfPixels stats1 + regionNumberOfPixels stats2
--             }
--         )
--         (regions1 <> regions2)
-- makeRegion :: Manifest a -> Extent -> Region a
-- makeRegion =
-- stdAvg :: (Real a, VS.Storable a) => Manifest a -> Double


groupRegionsToManifest :: VS.Storable a => [GroupRegion a] -> Size -> Manifest a
groupRegionsToManifest groupRegions imgSize = unsafePerformIO $ do
    mutManifest :: MutableManifest c a <- new imgSize
    mapM_
        (\groupRegion ->
            mapM_
                (writeExt mutManifest (regionAverage $ groupRegionStats groupRegion) . regionExtent)
                (childrenRegions groupRegion)
        )
        groupRegions

    unsafeFreeze mutManifest

    where writeExt mutImg value ext =
            let (n, s, e, w) = takeExtent ext
                points = [(y, x) | y <- [s..n-1], x <- [w..e-1]]
            in mapM_ (\(y, x) -> write mutImg (ix2 (n - 1 - y) x) value) points



regionSplit :: forall a v. (Real a, VS.Storable a) =>
    (Manifest a -> Bool) -- ^ Predicate to check if region is homogeneous
    -> (Manifest a -> v) -- ^ Function to create region value from region
    -> Manifest a -- ^ Image to split
    -> QuadTree (v, Int) -- ^ QuadTree with region values and segment numbers
regionSplit isHomogeneous makeRegionValue img  = fst $ makeTree (ix2 0 0) imgSize 0
    where
        imgSize@(Z :. imgHeight :. imgWidth) = manifestSize img
        makeTree :: Point -> Size -> Int -> (QuadTree (v, Int), Int)
        makeTree point@(Z :. y :. x) (Z :. h :. w) currentSegment
            | y < 0 || x < 0 || y + h  > imgHeight || x + w > imgWidth = error "Invalid region"
            | h == 1 || w == 1 =
                let onePixRegion = crop (Rect x y w h) img in
                (TLeaf (makeRegionValue onePixRegion, currentSegment), currentSegment + 1)
            | otherwise =
                let region = crop (Rect x y w h) img

                in
                    if isHomogeneous region
                    then (TLeaf (makeRegionValue region, currentSegment), currentSegment + 1)
                    else
                        let halfH = ceiling (fromIntegral h / 2 :: Double)
                            halfW = ceiling (fromIntegral w / 2 :: Double)
                            (nw, segmentN1) = makeTree point (ix2 halfH halfW) currentSegment
                            (ne, segmentN2) = makeTree (ix2 y (x + halfW)) (ix2 halfH  (w - halfW)) segmentN1
                            (sw, segmentN3) = makeTree (ix2 (y + halfH) x) (ix2 (h - halfH) halfW) segmentN2
                            (se, segmentN4) = makeTree (ix2 halfH halfW) (ix2 (h - halfH) (w - halfW)) segmentN3
                        in (TNode nw ne sw se, segmentN4)




-- | Perform merge step in 'split/merge' algorithm
mergeRegions :: forall el region.
    (el -> Extent -> region) -- ^ Function to create region from element and extent
    -> (el -> Extent -> region -> Either RegionError region) -- ^ Function to merge two regions
    -- -> (region -> region -> region) -- ^ Function to merge two regions
    -- -> (region -> Bool) -- ^ Predicate to check if region is homogeneous
    -- -> (region -> Bool) -- ^ Predicate to check if region is connected
    -> QuadTree el -- ^ QuadTree to merge
    -> Extent -- ^ Extent of the QuadTree
    -> [region] -- ^ List of merged regions

mergeRegions singletonRegion tryAddToGroup quadTree extent =
    Prelude.foldr accum [] (dfsQuadTreeWithExt quadTree extent)
    where
        accum (el, ext) groups =
            let elementaryRegion = singletonRegion el ext
                containerRegions =
                    rights $ List.map (tryAddToGroup el ext) groups
            in
                case containerRegions of
                    region:_ -> region : groups
                    []    -> elementaryRegion : groups


-- mergeRegions singletonRegion mergeTwo isHomogeneous isConnected quadTree extent =
--     Prelude.foldr accum [] (dfsQuadTreeWithExt quadTree extent)
--     where
--         accum (el, ext) groups =
--             let elementaryRegion = singletonRegion el ext
--                 containerRegion =
--                     List.find (\groupRegion -> isHomogeneous groupRegion && isConnected groupRegion)
--                     $ List.map (mergeTwo elementaryRegion) groups
--             in
--                 case containerRegion of
--                     Just region -> region : groups
--                     Nothing     -> elementaryRegion : groups
saveBMP :: (VS.Storable a, Convertible a RGBPixel) => FilePath -> Manifest a -> IO ()
saveBMP path img =
    saveRGB path (I.map convert img)

saveRGB :: FilePath -> Manifest RGBPixel -> IO ()
saveRGB path img =
    writeBitmap path $ toJuicyRGB img

{-
mergeRegions :: QuadTree el -> Extent -> (el -> Extent -> region) -> (region -> region -> region) -> (region -> Bool) -> AdjacencyMap region
mergeRegions quadTree extent regionFrom mergeRegions isHomogeneous = foldr accum (empty, Nothing) (dfsQuadTreeWithExt quadTree extent)
    where   accum (el, ext) (graph, Nothing) = (graph, Just (regionFrom el ext))
            accum (el, ext) (graph, Just currentRegion) =
                let newRegion = regionFrom el ext
                    mergedRegion = mergeRegions currentRegion newRegion in

                if isHomogeneous mergedRegion
                then (graph `overlay` (), Just newRegion)
                else case graph of
                    Empty -> (vertices [region], Just region)
                    _ -> let newRegion = regionFrom el ext
                             newGraph = overlay (vertices [newRegion]) graph
                             newGraph' = foldr (\v g -> connect v newRegion g) newGraph (vertices $ filter (not . isHomogeneous) $ dfsQuadTreeWithExt quadTree extent)
                         in (newGraph', Just newRegion) -}

{- regionSplit :: (Real a, VS.Storable a) => Manifest a -> (Manifest a -> Bool) -> QuadTree (Point, Size, Int)
regionSplit img isHomogeneous = fst $ makeTree (ix2 0 0) imgSize 0
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
                    else (TNode nw ne sw se, segmentN4) -}
