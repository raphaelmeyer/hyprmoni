{-# LANGUAGE OverloadedStrings #-}

module UI (draw, attributes, Name, State (..)) where

import qualified Brick as Util
import qualified Brick.AttrMap as Attr
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Core as Core
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import qualified Monitor
import qualified Selection

appTitle :: Text.Text
appTitle = "<hyprmoni>"

data State = State
  { sMonitors :: [Monitor.Info],
    sSelected :: Selection.Selection
  }

data Name = Name Text.Text deriving (Eq, Ord, Show)

draw :: State -> [T.Widget Name]
draw state =
  [ Core.hBox
      [ Border.borderWithLabel
          (Core.txt appTitle)
          ( Core.padLeft (Core.Pad 1) . Core.padRight (Core.Pad 2) . Core.padTopBottom 1 . Core.hBox $
              map (drawMonitor $ sSelected state) (sMonitors state)
          )
      ]
  ]

drawMonitor :: Selection.Selection -> Monitor.Info -> T.Widget Name
drawMonitor selection monitor =
  Core.padLeft (Core.Pad 1)
    . Border.borderWithLabel (drawMonitorBorder monitor selected)
    . Core.hLimit 16
    . Core.vBox
    $ [ drawCurrentMode monitor,
        Border.hBorder,
        drawModes selection monitor
      ]
  where
    selected = Selection.selectedMonitor selection == Monitor.name monitor

drawMonitorBorder :: Monitor.Info -> Bool -> T.Widget Name
drawMonitorBorder monitor selected =
  if selected
    then Core.withAttr aSelected title
    else title
  where
    title = Core.padLeftRight 1 . Core.txt . Monitor.name $ monitor

drawCurrentMode :: Monitor.Info -> T.Widget Name
drawCurrentMode = Core.padTop (Core.Pad 1) . Center.hCenter . Core.txt . Monitor.mode

drawModes :: Selection.Selection -> Monitor.Info -> T.Widget Name
drawModes selection monitor =
  let name = Monitor.name monitor
      available = Monitor.available monitor
      selected =
        if name == Selection.selectedMonitor selection
          then Just $ Selection.selectedMode selection
          else Nothing
   in Core.vBox
        [ Core.vLimit 3 . Core.viewport (Name name) T.Vertical . Core.vBox $
            map (drawMode selected) available
        ]

drawMode :: Maybe Text.Text -> Text.Text -> T.Widget n
drawMode selected mode =
  Core.padRight (Core.Pad 1)
    . Core.padLeft Core.Max
    . Core.withAttr style
    . visible
    $ Core.txt mode
  where
    style = if Just mode == selected then aSelected else aDefault
    visible = if Just mode == selected then Core.visible else id

aSelected :: Attr.AttrName
aSelected = Attr.attrName "selected"

aDefault :: Attr.AttrName
aDefault = Attr.attrName "default"

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    [(aSelected, Util.fg Vty.magenta)]
