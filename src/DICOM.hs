{-# LANGUAGE PatternSynonyms #-}
module DICOM where
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString.Lazy            as BS
import qualified Data.ByteString.Lazy.Char8      as BSChar
import           Data.Dicom
import           Data.Either.Combinators
import           Data.Endian
import           Data.Int                        (Int16)
import qualified Data.List                       as L
import qualified Data.Vector.Storable            as VS
import           Data.Vector.Storable.ByteString
import           Vision.Image.Type               (Manifest (..))
import           Vision.Primitive.Shape          (ix2)

pattern PixelData  :: Tag
pattern PixelData = Tag 0x7FE0 0x0010
pattern Rows :: Tag
pattern Rows = Tag 0x0028 0x0010
pattern Columns :: Tag
pattern Columns = Tag 0x0028 0x0011
pattern BitsAllocated :: Tag
pattern BitsAllocated = Tag 0x0028 0x0100
pattern BitsStored :: Tag
pattern BitsStored = Tag 0x0028 0x0101
pattern HighBit :: Tag
pattern HighBit = Tag 0x0028 0x0102
pattern PixelRepresentation :: Tag
pattern PixelRepresentation = Tag 0x0028 0x0103
pattern PhotometricInterpretation :: Tag
pattern PhotometricInterpretation = Tag 0x0028 0x0004


manifestFromDICOM :: [DicomElement] -> Either String (Manifest Word16)
manifestFromDICOM dicomEls = do
  rows <- findEl Rows
  columns <- findEl Columns
  pixData <- findEl PixelData
  photometricInterpret <- findEl PhotometricInterpretation

  pixelRepr <- findEl PixelRepresentation
  let   word16vector =
            VS.map ((fromIntegral :: Int16 -> Word16 ) . toLittleEndian) $ byteStringToVector (BS.toStrict $ elementValue pixData)
            -- VS.map ((fromIntegral :: Int16 -> Word16)) $ byteStringToVector (BS.toStrict $ elementValue pixData)
--   pixelInterpetStr <- string (elementValue photometricInterpret)
        pixelInterpetValue = BSChar.unpack (elementValue photometricInterpret)


  if "MONOCHROME2" `L.isInfixOf` pixelInterpetValue then
    Right
        Manifest {
        manifestSize =
            ix2 (fromIntegral $ word16 $ elementValue rows)
                (fromIntegral $ word16 $ elementValue columns),
        manifestVector = word16vector}
  else Left ("Image is not monochrome. Actual photometric interpretation: " ++ pixelInterpetValue)
  where
    word16 :: BS.ByteString -> Word16
    word16 = runGet getWord16le

    string :: BS.ByteString -> Either String String
    string = mapBoth (\(_,_,errorMsg) -> errorMsg) (\ (_,_,value) -> value) . decodeOrFail

    findEl tag = maybe (Left ("Could not find tag " ++ show tag)) Right $ L.find (\DicomElement {elementTag} -> elementTag == tag) dicomEls
