module Histogram where
import           Foreign.Storable (Storable)
import           Vision.Image     as I

windowLevel :: (Real a, Ord a, Convertible Float a, Storable a) => a -> a -> a -> a -> Manifest a -> Manifest a
windowLevel wCenter wWidth newMax newMin img = I.map f img where
    f pixel | float pixel < minThresh = newMin
            | float pixel > maxThresh = newMax
            | otherwise = newMin + convert ((float (newMax - newMin) * (float pixel - minThresh)) / float wWidth)
    minThresh = float wCenter - float wWidth / 2
    maxThresh = float wCenter + float wWidth / 2
    float x = realToFrac x :: Float

