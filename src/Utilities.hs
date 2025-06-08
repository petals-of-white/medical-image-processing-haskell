module Utilities where
import           Codec.Picture.Bitmap     (writeBitmap)
import           Foreign.Storable         (Storable)
import           Vision.Image             as I
import           Vision.Image.JuicyPixels (toJuicyRGB)


saveBMP :: (Storable a, Convertible a RGBPixel) => FilePath -> Manifest a -> IO ()
saveBMP path img = saveRGB path (I.map convert img)

saveRGB :: FilePath -> Manifest RGBPixel -> IO ()
saveRGB path img = writeBitmap path $ toJuicyRGB img
