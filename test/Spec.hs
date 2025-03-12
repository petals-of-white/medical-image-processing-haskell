module Spec where
import           Data.List                    as List
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Maybe                   (fromJust)
import           Data.Set                     as Set
import           Data.Word                    (Word16)
import           Graphics.Gloss.Data.Extent   (Extent, makeExtent)
import           Graphics.Gloss.Data.QuadTree
import           Instances                    ()
import           QuadTree
import           Segmentation
import           Test.QuickCheck              as QuickCheck
import           Vision.Image
import           Vision.Primitive


prop_splitMakesAllPixelsSegmented :: Word16 -> Manifest Word16 -> Property
prop_splitMakesAllPixelsSegmented  stdThresh img@Manifest{manifestSize = Z :. height :. width} =
    classify (height == 1 || width == 1) "1pix thin image" $ 
    counterexample ("Failed at number of segments :" ++ show (length segments) ++ ". " ++ show segments) $
    Set.fromList (coordsInExtent imgExtent) == Set.fromList (coordsInQuadTree segmented imgExtent)
    where   imgExtent = makeExtent height 0 width 0
            segmented = Segmentation.regionSplit ((<=  stdThresh) . regionStd . makeStats) makeStats img
            -- stdThresh = 12
            makeStats = fromJust . Segmentation.computeStats
            segments = snd <$> dfsQuadTreeWithExt segmented imgExtent


prop_mergeMakesAllPixelsSegmented :: Word16 -> Word16 ->  Manifest Word16 -> Property
prop_mergeMakesAllPixelsSegmented stdThresh avgDiffThresh img@Manifest{manifestSize = Z :. height :. width} =
    -- counterexample ("Failed at number of segments :" ++ show (length segments) ++ ". " ++ show segments) $
    classify (height == 1 || width == 1) "1pix thin image" $ 
    Set.fromList (coordsInExtent imgExtent) === Set.fromList coordsAfterMerge

    where   imgExtent = makeExtent height 0 width 0
            splitted = Segmentation.regionSplit ((<=  stdThresh) . regionStd . makeStats) makeStats img
            merged = Segmentation.mergeRegions1 avgDiffThresh splitted imgExtent
            -- avgDiffThresh = 5
            -- stdThresh = 12
            makeStats = fromJust . Segmentation.computeStats
            -- segments = snd <$> dfsQuadTreeWithExt splitted imgExtent
            coordsAfterMerge =
                concat $
                concatMap
                    (List.map (coordsInExtent . regionExtent) . NonEmpty.toList .  childrenRegions)
                    merged

prop_groupsContainSameSegments :: Word16 -> Word16 -> Manifest Word16 -> Property
prop_groupsContainSameSegments stdThresh avgDiffThresh img@Manifest{manifestSize = Z :. height :. width} =
    -- counterexample ("Failed at number of segments :" ++ show (length segments) ++ ". " ++ show segments) $
    classify (height == 1 || width == 1) "1pix thin image" $ 
    counterexample ("grouped regions:\n" ++
        unlines (List.map showGroup merged)
        ++ "Regions(" ++  show (Set.size nonIncludedRegions) ++
        ") not included after merge: " ++ unlines (List.map showRegion (Set.toList nonIncludedRegions)))
    $
    Set.fromList elementaryRegions == Set.fromList regionsAfterMerge

    where   imgExtent = makeExtent height 0 width 0
            splitted = Segmentation.regionSplit ((<=  stdThresh) . regionStd . makeStats) makeStats img
            merged = Segmentation.mergeRegions1 avgDiffThresh splitted imgExtent
            -- avgDiffThresh = 5
            -- stdThresh = 12
            makeStats = fromJust . Segmentation.computeStats
            elementaryRegions = (\((info, segId), ext) -> Region segId info ext) <$> dfsQuadTreeWithExt splitted imgExtent
            regionsAfterMerge = concatMap (NonEmpty.toList . childrenRegions) merged
            nonIncludedRegions = Set.fromList elementaryRegions `Set.difference` Set.fromList regionsAfterMerge

prop_noPixelOverlap :: Word16 -> Manifest Word16 -> Property
prop_noPixelOverlap thresh img@Manifest{manifestSize = Z :. height :. width} =
    classify (height == 1 || width == 1) "1pix thin image" $ 
    counterexample ("Failed at number of segments :" ++ show (length segments) ++ ". " ++ show segments) $
    nub coveredPixels == coveredPixels
    where   imgExtent = makeExtent height 0 width 0
            segmented = Segmentation.regionSplit ((<=  thresh) . regionStd . makeStats) makeStats img
            segments = snd <$> dfsQuadTreeWithExt segmented imgExtent
            -- thresh = 12
            makeStats = fromJust . Segmentation.computeStats
            coveredPixels = coordsInQuadTree segmented imgExtent

prop_uniqueGroupIdsAfterMerge :: Word16 -> QuadTree (RegionStatistics Word16, Int) -> Extent -> Property
prop_uniqueGroupIdsAfterMerge thresh quadTree extent = 
    counterexample (unlines (List.map showGroup mergedGroups)) $ 
    within (4 * 1000_000) $
    nub (groupId <$> mergedGroups) == (groupId <$> mergedGroups)
    where mergedGroups = mergeRegions1 thresh quadTree extent



main :: IO ()
main = do
    putStrLn "All pixels are segmented after split?"
    quickCheck prop_splitMakesAllPixelsSegmented

    putStrLn "No pixel overlapping after split?"
    quickCheck prop_noPixelOverlap

    putStrLn "All pixels are segmented after merge?"
    quickCheck prop_mergeMakesAllPixelsSegmented
    putStrLn "All group ids unique after merge?"

    quickCheckWith stdArgs{maxSize=14} prop_uniqueGroupIdsAfterMerge

    putStrLn "Groups contain all segments?"
    quickCheck prop_groupsContainSameSegments
