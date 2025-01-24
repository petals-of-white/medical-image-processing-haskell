{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module DICOM where
import           Control.Monad                   ((<=<))
import           Core
import           Data.Binary
import qualified Data.ByteString.Lazy            as BS
import qualified Data.ByteString.Lazy.Char8      as BSChar
import           Data.Dicom
import           Data.Either.Combinators
import qualified Data.List                       as L
import           Data.Text                       (pack, unpack)
import qualified Data.Vector.Storable            as VS
import           Data.Vector.Storable.ByteString
import           Graphics.UI.TinyFileDialogs
import           Vision.Image.Type               (Manifest (..))
import           Vision.Primitive.Shape          (ix2)
import Data.Binary.Get
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


selectFile :: IO AppEvent
selectFile = do
    selectedFiles <- openFileDialog "Виберіть DICOM файл" "" ["*.dcm"] "DICOM files" False
    case selectedFiles of
        Just [selectedFile] -> return (LoadDicom (unpack selectedFile))
        _                   -> return NoOp

manifestFromDICOM :: [DicomElement] -> Either String (Manifest Word16)
manifestFromDICOM dicomEls = do
  rows <- findEl Rows
  columns <- findEl Columns
  pixData <- findEl PixelData
  photometricInterpret <- findEl PhotometricInterpretation

  pixelRepr <- findEl PixelRepresentation
  let   word16vector = byteStringToVector (BS.toStrict $ elementValue pixData)
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

loadDicomFromFile :: FilePath -> IO AppEvent
loadDicomFromFile filepath = do
    manifestImg <- (manifestFromDICOM <=< parseDicomFile) <$> readDicomFile filepath
    case manifestImg of
        Right d -> do
            notifyPopup "Info" (pack $ "Got image: " ++ show (manifestSize d) ++ " " ++ show (VS.length (manifestVector d))) Info
            return (SetOriginal d)
        Left errorMsg -> do
            notifyPopup "Error" (pack errorMsg) Error
            return NoOp
