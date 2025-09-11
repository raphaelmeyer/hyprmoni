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
import qualified Monitor

data Selection = Selection
  { selectedMonitor :: Text.Text,
    selectedMode :: Monitor.Mode
  }

first :: [Monitor.Info] -> Selection
first monitors = Selection (Monitor.name . head $ monitors) (Monitor.mode . head $ monitors)

previousMonitor :: [Monitor.Info] -> Selection -> Selection
previousMonitor (left : right : ms) selection
  | Monitor.name left == selectedMonitor selection = selection
  | Monitor.name right == selectedMonitor selection = Selection (Monitor.name left) (Monitor.mode left)
  | otherwise = previousMonitor (right : ms) selection
previousMonitor _ selection = selection

nextMonitor :: [Monitor.Info] -> Selection -> Selection
nextMonitor (left : right : ms) selection
  | Monitor.name left == selectedMonitor selection = Selection (Monitor.name right) (Monitor.mode right)
  | otherwise = nextMonitor (right : ms) selection
nextMonitor _ selection = selection

previousMode :: [Monitor.Info] -> Selection -> Selection
previousMode monitors (Selection monitor mode) = Selection monitor previous
  where
    previous = modePrevious' mode available
    available = Monitor.available (monitorByName monitor monitors)

modePrevious' :: Monitor.Mode -> [Monitor.Mode] -> Monitor.Mode
modePrevious' mode (top : next : rest)
  | top == mode = mode
  | next == mode = top
  | otherwise = modePrevious' mode (next : rest)
modePrevious' mode _ = mode

nextMode :: [Monitor.Info] -> Selection -> Selection
nextMode monitors (Selection monitor mode) = Selection monitor next
  where
    next = modeNext' mode available
    available = Monitor.available (monitorByName monitor monitors)

modeNext' :: Monitor.Mode -> [Monitor.Mode] -> Monitor.Mode
modeNext' mode (top : next : rest)
  | top == mode = next
  | otherwise = modeNext' mode (next : rest)
modeNext' mode _ = mode

monitorByName :: Text.Text -> [Monitor.Info] -> Monitor.Info
monitorByName name (m : ms) = if Monitor.name m == name then m else monitorByName name ms
monitorByName _ [] = undefined
