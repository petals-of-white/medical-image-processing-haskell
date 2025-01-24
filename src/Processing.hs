{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Processing where

import qualified Data.ByteString                 as BS
import           Data.Vector.Storable.ByteString
import           Data.Word                       (Word16, Word8)
import           Vision.Histogram
import           Vision.Image                    as Img
import           Vision.Primitive                (DIM1, ix1)


convertNormalized :: Word16 -> Word8
convertNormalized v = round ((fromIntegral v / fromIntegral (maxBound :: Word16) :: Float) * fromIntegral (maxBound :: Word8))


manifestToRGBAbs :: Manifest Word16 -> BS.ByteString
manifestToRGBAbs = vectorToByteString . manifestVector . compute . convertViaWord8 . Img.delay
  where
    convertViaWord8 :: Delayed Word16 -> Delayed RGBAPixel
    convertViaWord8 imgw16 = convert (Img.map (GreyPixel . convertNormalized) imgw16 :: Delayed GreyPixel)


manifestWord16ToRGBA :: Manifest Word16 -> Manifest RGBAPixel
manifestWord16ToRGBA = compute . convertViaWord8 . Img.delay
  where
    convertViaWord8 :: Delayed Word16 -> Delayed RGBAPixel
    convertViaWord8 imgw16 = convert (Img.map (GreyPixel . convertNormalized) imgw16 :: Delayed GreyPixel)

instance ToHistogram Word16 where
    type PixelValueSpace Word16 = DIM1
    pixToIndex val = ix1 (fromIntegral val)
    domainSize _ = ix1 (fromIntegral (maxBound :: Word16))

makeViewImage :: Manifest Word16 -> Bool -> Bool -> Manifest RGBAPixel
makeViewImage sourceImg shouldFilter shouldSegment =

  case (sourceImg, shouldFilter, shouldSegment) of
    (source, False, False) -> manifestWord16ToRGBA $ equalizeImage source
    (source, True, False) ->
      let fltred :: Manifest Word16 = blur 3 source in
        manifestWord16ToRGBA  $
            equalizeImage fltred
    (source, False, True) ->
      manifestWord16ToRGBA $ equalizeImage $
        otsu (BinaryThreshold maxBound minBound) (Img.map (GreyPixel .convertNormalized) source :: Manifest GreyPixel)
    (source, True, True) ->
      manifestWord16ToRGBA $ equalizeImage $
        otsu (BinaryThreshold maxBound minBound) (Img.map (GreyPixel .convertNormalized) (blur 3 source :: Manifest Word16) :: Manifest GreyPixel)

rgbaManifestToBS :: Manifest RGBAPixel -> BS.ByteString
rgbaManifestToBS = vectorToByteString . manifestVector
