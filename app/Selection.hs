module Selection
  ( Selection (..),
    first,
    nextMode,
    nextMonitor,
    previousMode,
    previousMonitor,
  )
where

import qualified Data.Text as Text
import qualified Types

data Selection = Selection
  { selectedMonitor :: Text.Text,
    selectedMode :: Text.Text
  }

first :: [Types.MonitorInfo] -> Selection
first monitors = Selection (Types.name . head $ monitors) (Types.mode . head $ monitors)

previousMonitor :: [Types.MonitorInfo] -> Selection -> Selection
previousMonitor (left : right : ms) selection
  | Types.name left == selectedMonitor selection = selection
  | Types.name right == selectedMonitor selection = Selection (Types.name left) (Types.mode left)
  | otherwise = previousMonitor (right : ms) selection
previousMonitor _ selection = selection

nextMonitor :: [Types.MonitorInfo] -> Selection -> Selection
nextMonitor (left : right : ms) selection
  | Types.name left == selectedMonitor selection = Selection (Types.name right) (Types.mode right)
  | otherwise = nextMonitor (right : ms) selection
nextMonitor _ selection = selection

previousMode :: [Types.MonitorInfo] -> Selection -> Selection
previousMode monitors (Selection monitor mode) = Selection monitor previous
  where
    previous = modePrevious' mode available
    available = Types.available (monitorByName monitor monitors)

modePrevious' :: Text.Text -> [Text.Text] -> Text.Text
modePrevious' mode (top : next : rest)
  | top == mode = mode
  | next == mode = top
  | otherwise = modePrevious' mode (next : rest)
modePrevious' mode _ = mode

nextMode :: [Types.MonitorInfo] -> Selection -> Selection
nextMode monitors (Selection monitor mode) = Selection monitor next
  where
    next = modeNext' mode available
    available = Types.available (monitorByName monitor monitors)

modeNext' :: Text.Text -> [Text.Text] -> Text.Text
modeNext' mode (top : next : rest)
  | top == mode = next
  | otherwise = modeNext' mode (next : rest)
modeNext' mode _ = mode

monitorByName :: Text.Text -> [Types.MonitorInfo] -> Types.MonitorInfo
monitorByName name (m : ms) = if Types.name m == name then m else monitorByName name ms
monitorByName _ [] = undefined
