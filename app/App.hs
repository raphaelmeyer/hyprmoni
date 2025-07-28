{-# LANGUAGE OverloadedStrings #-}

module App (run) where

import qualified Brick.AttrMap as Attr
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Core as Core
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import qualified Types

data State = State
  { sMonitors :: [Types.MonitorInfo]
  }

data Name = Name Text.Text deriving (Eq, Ord, Show)

appTitle :: Text.Text
appTitle = "hyprmoni"

mkApp :: M.App State e Name
mkApp =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = pure (),
      M.appAttrMap = const attributes
    }

run :: [Types.MonitorInfo] -> IO ()
run monitors = do
  _ <- M.defaultMain mkApp (State monitors)
  pure ()

draw :: State -> [T.Widget Name]
draw state =
  [ Core.hBox
      [ Border.borderWithLabel
          (Core.txt appTitle)
          ( Core.padLeft (Core.Pad 1) . Core.padRight (Core.Pad 2) . Core.padTopBottom 1 . Core.hBox $
              map drawMonitor (sMonitors state)
          )
      ]
  ]

drawMonitor :: Types.MonitorInfo -> T.Widget Name
drawMonitor monitor =
  Core.padLeft (Core.Pad 1)
    . Border.borderWithLabel (Core.txt . Types.name $ monitor)
    . Core.vLimit 5
    . Core.hLimit 16
    . Center.center
    $ drawModes monitor

drawModes :: Types.MonitorInfo -> T.Widget Name
drawModes monitor =
  let name = Types.name monitor
      current = Types.mode monitor
      available = Types.available monitor
   in Core.vBox
        [ Core.vLimit 3 . Core.viewport (Name name) T.Vertical . Core.vBox $
            map (drawMode current) available
        ]

drawMode :: Text.Text -> Text.Text -> T.Widget n
drawMode current mode =
  Core.padRight (Core.Pad 1)
    . Core.padLeft Core.Max
    . (if mode == current then Core.visible else id)
    $ Core.txt mode

handleEvent :: T.BrickEvent Name e -> T.EventM Name State ()
handleEvent (T.VtyEvent e) = case e of
  Vty.EvKey Vty.KEsc [] -> M.halt
  Vty.EvKey (Vty.KChar 'q') [] -> M.halt
  _ -> pure ()
handleEvent _ = pure ()

attributes :: Attr.AttrMap
attributes = Attr.attrMap Vty.defAttr []
