module Core where
import           Data.Word    (Word16)
import           Vision.Image

data AppModel = AppModel {source :: Maybe (Manifest Word16), applyFilter :: Bool, applySegmentation :: Bool} deriving (Show, Eq)

data AppEvent = NoOp | AppInit | OpenSelectFileDialog | LoadDicom FilePath | SetOriginal (Manifest Word16) | Clear