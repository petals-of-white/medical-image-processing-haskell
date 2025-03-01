
module Segmentation where

import qualified Control.Foldl                as Foldl
import           Data.Either                  (rights)
import           Data.List                    as List
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  (Any (Any))
import qualified Data.Vector.Storable         as VS
import           Graphics.Gloss.Data.Extent
import           Graphics.Gloss.Data.QuadTree
import           QuadTree
import           System.IO.Unsafe             (unsafePerformIO)
import           Vision.Image                 as I
import           Vision.Primitive


data RegionStatistics pix = RegionStatistics {
    regionMin            :: pix,
    regionMax            :: pix,
    regionAverage        :: pix,
    regionStd            :: pix,
    regionSumIntensity   :: Int,
    regionNumberOfPixels :: Int
    } deriving (Functor, Show, Eq, Ord)



data Region info = Region {regionId :: Int, regionInfo :: info, regionExtent :: Extent}
    deriving (Eq, Ord, Show)


data GroupRegion info = GroupRegion {
    groupId         :: Int,
    groupInfo       :: info,
    childrenRegions :: NonEmpty (Region info)
    } deriving (Eq, Show)

data RegionError = NotHomogeneous | NotConnected

instance Ord Extent where

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
        maybeStats mMin mMax mMean mStd mSum mLen =
            fmap (fmap truncate) $ RegionStatistics <$> mMin <*> mMax <*> mMean <*> mStd <*> mSum <*> mLen
        folder  =
            maybeStats <$> Foldl.minimum <*> Foldl.maximum
            <*> fmap Just Foldl.mean <*> fmap Just Foldl.std
            <*> fmap (Just . round) Foldl.sum
            <*> pure (Just $ shapeLength (manifestSize img))


fromSingleRegion :: Int -> Region v -> GroupRegion v
fromSingleRegion groupId region = GroupRegion groupId (regionInfo region) (region :| [])

statisticsAverage :: Num a => RegionStatistics a -> a
statisticsAverage RegionStatistics {regionSumIntensity=sumI, regionNumberOfPixels=nPix} = fromIntegral (sumI `div` nPix)


regionAverage' :: Num a => Region (RegionStatistics a) -> a
regionAverage' Region {
    regionInfo = RegionStatistics {
        regionSumIntensity=sumI,
        regionNumberOfPixels=nPix}
    } = fromIntegral (sumI `div` nPix)

groupAverage :: Num a => GroupRegion (RegionStatistics a) -> a
groupAverage GroupRegion {
    groupInfo = RegionStatistics {
        regionSumIntensity=sumI,
        regionNumberOfPixels=nPix}
    } = fromIntegral (sumI `div` nPix)
tryMergeByDiffTreshold :: forall a. Integral a =>
    a -- ^ Threshold
    -> Region (RegionStatistics a)
    -> GroupRegion (RegionStatistics a)
    -> Either RegionError (GroupRegion (RegionStatistics a))
tryMergeByDiffTreshold avgDiffTreshold region groupRegion =
    if abs (statisticsAverage regStats - statisticsAverage groupStats) <= avgDiffTreshold
    then if region `isTouchingGroup` groupRegion
        then
            Right GroupRegion {
                groupId = groupId groupRegion,
                groupInfo = RegionStatistics {
                    regionMin = min (regionMin regStats) (regionMin groupStats),
                    regionMax = max (regionMax regStats) (regionMax groupStats),
                    regionAverage = round $ (float (regionAverage regStats) + float (regionAverage groupStats)) / 2,
                    regionStd = regionStd regStats + regionStd groupStats,
                    regionSumIntensity = regionSumIntensity groupStats + regionSumIntensity regStats,
                    regionNumberOfPixels = regionNumberOfPixels regStats + regionNumberOfPixels groupStats
                    },
                childrenRegions = region <| childrenRegions groupRegion
            }
        else Left NotConnected
    else Left NotHomogeneous

    where
        GroupRegion {groupInfo = groupStats} = groupRegion
        Region {regionInfo=regStats} = region
        float = fromIntegral :: a -> Float


isTouchingGroup :: Region a -> GroupRegion a -> Bool
isTouchingGroup Region{regionExtent=rExt} GroupRegion {childrenRegions=children} =
    let (Any result) = mconcat $ [  Any (childExt `extentNeighbours` rExt) |
                                    Region{regionExtent=childExt} <- toList children]
    in result


extentNeighbours :: Extent -> Extent -> Bool
extentNeighbours ext1 ext2 =
    let (n1, s1, e1, w1) = takeExtent ext1
        (n2, s2, e2, w2) = takeExtent ext2
    in  (n1 == s2 || s1 == n2) && overlaps w1 e1 w2 e2  -- Vertical neighbors
     || (e1 == w2 || w1 == e2) && overlaps s1 n1 s2 n2  -- Horizontal neighbors
  where
    overlaps min1 max1 min2 max2 = min1 <= max2 && min2 <= max1



groupRegionsToManifest :: (VS.Storable a, Num a) => [GroupRegion (RegionStatistics a)] -> Size -> Manifest a
groupRegionsToManifest groupRegions imgSize@(Z :. height :. _width) = unsafePerformIO $ do
    mutManifest :: MutableManifest c a <- new imgSize
    mapM_
        (\groupRegion ->
            mapM_
                (\Region{regionExtent} ->
                    writeRect mutManifest
                        (statisticsAverage $ groupInfo groupRegion)
                        (extentToRect regionExtent height)
                )
                (childrenRegions groupRegion)
        )
        groupRegions

    unsafeFreeze mutManifest

regionSplit :: forall a v. (Real a, VS.Storable a) =>
    (Manifest a -> Bool) -- ^ Predicate to check if region is homogeneous
    -> (Manifest a -> v) -- ^ Function to create region value from region
    -> Manifest a -- ^ Image to split
    -> QuadTree (v, Int) -- ^ QuadTree with region values and segment numbers
regionSplit isHomogeneous makeRegionValue img  = fst $ makeTree (ix2 0 0) imgSize 0
    where
        imgSize@(Z :. imgHeight :. imgWidth) = manifestSize img
        makeTree :: Point -> Size -> Int -> (QuadTree (v, Int), Int)
        makeTree (Z :. y :. x) (Z :. h :. w) currentSegment
            | y < 0 || x < 0 || y + h  > imgHeight || x + w > imgWidth = error "Invalid region"
            | w == 0 || h == 0 = (TNil, currentSegment)
            | h == 1 && w == 1 =
                let elementaryRegion = crop (Rect x y w h) img in
                (TLeaf (makeRegionValue elementaryRegion, currentSegment), currentSegment + 1)
            | otherwise =
                let region = crop (Rect x y w h) img
                in
                    if isHomogeneous region
                    then (TLeaf (makeRegionValue region, currentSegment), currentSegment + 1)
                    else
                        let halfH = h `div` 2 + if odd h then 1 else 0
                            halfW = w `div` 2 + if odd w then 1 else 0

                            (nw, segmentN1) = makeTree (ix2 y x) (ix2 halfH halfW) currentSegment
                            (ne, segmentN2) = makeTree (ix2 y (x + halfW)) (ix2 halfH (w - halfW)) segmentN1
                            (sw, segmentN3) = makeTree (ix2 (y + halfH) x) (ix2 (h - halfH) halfW) segmentN2
                            (se, segmentN4) = makeTree (ix2 (y + halfH) (x + halfW)) (ix2 (h - halfH) (w - halfW)) segmentN3


                        in
                            -- if h == 1 then (TNode nw ne TNil TNil, segmentN4) -- horizontal single-pixel line
                            -- else if w == 1 then (TNode nw TNil sw TNil, segmentN4) -- vertical single-pixel line
                            -- else
                                 (TNode nw ne sw se, segmentN4) -- normal node

regionSplit' :: (VS.Storable a, Integral a) => a -> Manifest a -> QuadTree (RegionStatistics a, Int)
regionSplit' stdThresh = regionSplit ((<= stdThresh).  regionStd . makeStats) makeStats
    where makeStats = fromJust . computeStats


quadTreeToImage' :: (VS.Storable a, Num a) =>
    QuadTree (RegionStatistics a, Int) -- ^ QuadTree
    -> Size -- ^ Image size
    -> Manifest a
quadTreeToImage' = quadTreeToImage (\(stats,regId) ext -> regionAverage' (Region regId stats ext))

        -- let quadTree = regionSplit ((<= stdThresh).  regionStd . makeStats) makeStats img
        -- makeStats = fromJust . computeStats
showGroup :: GroupRegion a -> String
showGroup GroupRegion{groupId, childrenRegions} =
    "Group: id=" ++ show groupId ++ ", " ++ show (NonEmpty.length childrenRegions)
    ++ "child regions: " ++  concat (NonEmpty.intersperse ", "  (NonEmpty.map showRegion childrenRegions))

showRegion :: Region a -> String
showRegion Region{regionId, regionExtent} = "Region: id=" ++ show regionId ++ ". ext=" ++ show regionExtent
-- | Perform merge step in 'split/merge' algorithm
mergeRegions :: forall info.
    (Region info -> GroupRegion info -> Either RegionError (GroupRegion info)) -- ^ Function to merge two regions
    -> QuadTree (info, Int) -- ^ QuadTree to merge
    -> Extent -- ^ Extent of the QuadTree
    -> [GroupRegion info] -- ^ List of merged regions

mergeRegions tryAddToGroup quadTree extent =
    fst $ foldl addToGroup ([], 0) (dfsQuadTreeWithExt quadTree extent)

    where addToGroup (groups, currentGroupId) ((regInfo, regId), regExt) =
            let region = Region regId regInfo regExt
                potentialGroups = rights $ List.map (tryAddToGroup region) groups
            in
                case potentialGroups of
                    updatedGroup:_ ->
                        let withoutDuplicates =  List.filter (\gr -> groupId gr /= groupId updatedGroup) groups
                        in (updatedGroup : withoutDuplicates, currentGroupId)
                    [] ->
                        let singleRegionGroup = fromSingleRegion currentGroupId region
                        in (singleRegionGroup : groups, succ currentGroupId)

mergeRegions1 :: Integral a => a -> QuadTree (RegionStatistics a, Int) -> Extent -> [GroupRegion (RegionStatistics a)]
mergeRegions1 avgDiffthresh = mergeRegions (tryMergeByDiffTreshold avgDiffthresh)
