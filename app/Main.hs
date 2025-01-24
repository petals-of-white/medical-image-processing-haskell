{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Lib
import           Monomer
import           Core

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Haskell Image Processing",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel {source = Nothing, applyFilter = False, applySegmentation = False}
