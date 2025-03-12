{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module DICOM where
import           Control.Monad                  ((<=<))
import           Core
import           Data.Dicom
import           Data.Text                      (pack, unpack)
import qualified Data.Vector.Storable           as VS
import           "MedicalImageProcessing" DICOM
import           Graphics.UI.TinyFileDialogs
import           Vision.Image.Type              (Manifest (..))


selectFile :: IO AppEvent
selectFile = do
    selectedFiles <- openFileDialog "Виберіть DICOM файл" "" ["*.dcm"] "DICOM files" False
    case selectedFiles of
        Just [selectedFile] -> return (LoadDicom (unpack selectedFile))
        _                   -> return NoOp

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
