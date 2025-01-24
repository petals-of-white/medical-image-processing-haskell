{-# LANGUAGE OverloadedStrings #-}

module Lib where
import           Core
import qualified Data.ByteString        as BS
import           DICOM                  (loadDicomFromFile, selectFile)
import           Monomer
import           Vision.Image           as Img

import           Data.Text              (pack)
import           Processing
import           Vision.Primitive.Shape


-- VIEWS
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent

buildUI _wenv _model@AppModel {source=m, applyFilter, applySegmentation} = widgetTree where
  widgetTree = vstack [
      button "Load dicom" OpenSelectFileDialog,
      spacer,
      widgetMaybe m (\img ->
        let
            rgbView =  makeViewImage img applyFilter applySegmentation
            Z :. h :. w = manifestSize rgbView
            rgbaBS = rgbaManifestToBS rgbView in

        vstack [

            label $ pack $
              "Image loaded! Size: " ++ show (manifestSize img) ++ ", Raw length = " ++ show (BS.length rgbaBS),

            imageMem "Display Image" rgbaBS (Size (fromIntegral w) (fromIntegral h))
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
  Clear                -> [Model (model {source = Nothing})]
