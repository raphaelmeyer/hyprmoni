module App (run) where

import qualified Brick.AttrMap as Attr
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Core as Core
import qualified Graphics.Vty as Vty

data State = State

data Name = Name String deriving (Eq, Ord, Show)

mkApp :: M.App State e Name
mkApp =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = pure (),
      M.appAttrMap = const attributes
    }

initialState :: State
initialState = State

run :: IO ()
run = do
  _ <- M.defaultMain mkApp initialState
  pure ()

draw :: State -> [T.Widget Name]
draw _ =
  [ Core.hBox
      [ Border.borderWithLabel
          (Core.str "hyprmoni")
          ( Core.padLeft (Core.Pad 1) . Core.padRight (Core.Pad 2) . Core.padTopBottom 1 . Core.hBox $
              [ drawMonitor "eDP-1" "1920x1200",
                drawMonitor "HDMI-A" "3840x2160"
              ]
          )
      ]
  ]

drawMonitor :: String -> String -> T.Widget Name
drawMonitor name mode =
  Core.padLeft (Core.Pad 1)
    . Border.borderWithLabel (Core.str name)
    . Core.vLimit 5
    . Core.hLimit 16
    . Center.center
    . Core.vBox
    $ [ Core.vLimit 3 . Core.viewport (Name name) T.Vertical . Core.vBox $
          map
            (Core.padRight (Core.Pad 1) . Core.padLeft Core.Max)
            [ Core.str mode,
              Core.str "1000x800",
              Core.str "2000x1600",
              Core.str "3000x2400",
              if name == "HDMI-A" then Core.visible . Core.str $ "4000x3000" else Core.str "1234x567"
            ]
      ]

handleEvent :: T.BrickEvent Name e -> T.EventM Name State ()
handleEvent (T.VtyEvent e) = case e of
  Vty.EvKey Vty.KEsc [] -> M.halt
  Vty.EvKey (Vty.KChar 'q') [] -> M.halt
  _ -> pure ()
handleEvent _ = pure ()

attributes :: Attr.AttrMap
attributes = Attr.attrMap Vty.defAttr []
