{-# LANGUAGE OverloadedStrings #-}

module Lib where
import           Core
import qualified Data.ByteString          as BS
import           DICOM                    (loadDicomFromFile, selectFile)
import           Monomer
import           Vision.Image             as Img

import qualified Data.ByteString.Internal as BS
import           Data.Text                (pack)
import           Processing
import           Vision.Primitive.Shape


-- VIEWS
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent

buildUI _wenv _model@AppModel {source=m, applyFilter, applySegmentation} = widgetTree where
  widgetTree = vstack [
      button "Вибрати DICOM file..." OpenSelectFileDialog,
      spacer,
      hstack [
        label "Застосувати згладжування",
        checkboxV applyFilter UseFiltering,
        spacer,

        label "Застосувати порогову сегментацію",
        checkboxV applySegmentation UseSegmentation,
        spacer,

        button "Скинути зображення" Clear
      ],
      widgetMaybe m (\img ->
        let
            rgbView =  makeViewImage img applyFilter applySegmentation
            Z :. h :. w = manifestSize rgbView
            rgbaBS = rgbaManifestToBS rgbView
            imgId = pack $ "Image with ptr: " ++ show (fst (BS.toForeignPtr0 rgbaBS))
             in
        vstack [

            label $ pack $
              "Розмір: " ++ show (manifestSize img) ++ " " ++ show (BS.length rgbaBS),

            spacer,

            imageMem imgId rgbaBS (Size (fromIntegral w) (fromIntegral h))
        ]
      )
    ] `styleBasic` [padding 10]


-- EVENTS
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]

handleEvent _wenv _node model evt = case evt of
  NoOp                 -> []
  AppInit              -> []
  OpenSelectFileDialog -> [Task selectFile]
  LoadDicom path       -> [Task (loadDicomFromFile path)]
  SetOriginal img      -> [Model (model {source = Just img})]
  UseFiltering b       -> [Model (model {applyFilter =  b})]
  UseSegmentation b    -> [Model (model {applySegmentation = b})]
  Clear                -> [Model (model {source = Nothing})]
