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
import qualified Selection
import qualified Types

appTitle :: Text.Text
appTitle = "hyprmoni"

data State = State
  { sMonitors :: [Types.MonitorInfo],
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

drawMonitor :: Selection.Selection -> Types.MonitorInfo -> T.Widget Name
drawMonitor selection monitor =
  Core.padLeft (Core.Pad 1)
    . Border.borderWithLabel (drawMonitorBorder selection monitor)
    . Core.vLimit 5
    . Core.hLimit 16
    . Center.center
    $ drawModes (Selection.selectedMode selection) monitor

drawMonitorBorder :: Selection.Selection -> Types.MonitorInfo -> T.Widget Name
drawMonitorBorder selection monitor =
  if Selection.selectedMonitor selection == Types.name monitor
    then Core.withAttr aSelected title
    else title
  where
    title = Core.txt . Types.name $ monitor

drawModes :: Text.Text -> Types.MonitorInfo -> T.Widget Name
drawModes selected monitor =
  let name = Types.name monitor
      current = Types.mode monitor
      available = Types.available monitor
   in Core.vBox
        [ Core.vLimit 3 . Core.viewport (Name name) T.Vertical . Core.vBox $
            map (drawMode current selected) available
        ]

drawMode :: Text.Text -> Text.Text -> Text.Text -> T.Widget n
drawMode current selected mode =
  Core.padRight (Core.Pad 1)
    . Core.padLeft Core.Max
    . Core.withAttr style
    . visible
    $ Core.txt mode
  where
    style
      | mode == selected && mode == current = aSelected <> aCurrent
      | mode == selected = aSelected
      | mode == current = aCurrent
      | otherwise = aDefault
    visible = if mode == selected then Core.visible else id

aSelected :: Attr.AttrName
aSelected = Attr.attrName "selected"

aCurrent :: Attr.AttrName
aCurrent = Attr.attrName "current"

aDefault :: Attr.AttrName
aDefault = Attr.attrName "default"

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    [ (aSelected, Util.fg Vty.cyan),
      (aCurrent, Util.fg Vty.yellow),
      (aSelected <> aCurrent, Util.fg Vty.magenta)
    ]
