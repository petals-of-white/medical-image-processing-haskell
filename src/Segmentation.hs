module Segmentation where

import Graphics.Gloss.Data.QuadTree
import Vision.Image
import Vision.Primitive


isHomogeneous :: Manifest Int -> Bool
isHomogeneous = undefined

regionSplit :: Manifest Int -> QuadTree (Point, Size, Int)
regionSplit img = makeTree (ix2 0 0) imgSize 0
    where
        imgSize@(Z :. imgHeight :. imgWidth) = (manifestSize img)
        makeTree :: Point -> Size -> Int -> QuadTree (Point, Size, Int)
        makeTree point@(Z :. y :. x) size@(Z :. h :. w) prevSegment
            | h == 1 && w == 1 = TLeaf (point, size, prevSegment + 1)
            | otherwise =
                let region = crop (Rect x y w h) img
                    halfH = ceiling (fromIntegral h / 2)
                    halfW = ceiling (fromIntegral w / 2)
                    topLeft = makeTree point (ix2 halfH halfW) prevSegment
                    topRight = makeTree (point + (Z :. 0 :. halfW)) (ix2 halfH halfW) (prevSegment + 1)
                    bottomLeft = makeTree (point + (Z :. halfH :. 0)) (ix2 halfH halfW) (prevSegment + 2)
                    bottomRight = makeTree (point + (Z :. halfH :. halfW)) (ix2 halfH halfW) (prevSegment + 3)
                in  
                    if isHomogeneous region
                    then TLeaf (point, size, prevSegment + 1)
                    else
                        if
                        TNode topLeft topRight bottomLeft bottomRight
