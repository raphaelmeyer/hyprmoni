{-# LANGUAGE OverloadedStrings #-}

module App (run) where

import qualified Brick as Util
import qualified Brick.AttrMap as Attr
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Core as Core
import qualified Control.Monad.IO.Class as Monad
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import qualified Hyprland
import qualified Types

data Selection = Selection
  { selectedMonitor :: Text.Text,
    selectedMode :: Text.Text
  }

data State = State
  { sMonitors :: [Types.MonitorInfo],
    sSelected :: Selection
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
  _ <- M.defaultMain mkApp (initialState monitors)
  pure ()

initialState :: [Types.MonitorInfo] -> State
initialState monitors = State monitors (Selection (Types.name . head $ monitors) (Types.mode . head $ monitors))

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

drawMonitor :: Selection -> Types.MonitorInfo -> T.Widget Name
drawMonitor selection monitor =
  Core.padLeft (Core.Pad 1)
    . Border.borderWithLabel (drawMonitorBorder selection monitor)
    . Core.vLimit 5
    . Core.hLimit 16
    . Center.center
    $ drawModes (selectedMode selection) monitor

drawMonitorBorder :: Selection -> Types.MonitorInfo -> T.Widget Name
drawMonitorBorder selection monitor =
  if selectedMonitor selection == Types.name monitor
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

monitorPrevious :: State -> State
monitorPrevious (State monitors selection) = State monitors (monitorPrevious' monitors selection)

monitorPrevious' :: [Types.MonitorInfo] -> Selection -> Selection
monitorPrevious' (left : right : ms) selection
  | Types.name left == selectedMonitor selection = selection
  | Types.name right == selectedMonitor selection = Selection (Types.name left) (Types.mode left)
  | otherwise = monitorPrevious' (right : ms) selection
monitorPrevious' _ selection = selection

monitorNext :: State -> State
monitorNext (State monitors selection) = State monitors (monitorNext' monitors selection)

monitorNext' :: [Types.MonitorInfo] -> Selection -> Selection
monitorNext' (left : right : ms) selection
  | Types.name left == selectedMonitor selection = Selection (Types.name right) (Types.mode right)
  | otherwise = monitorNext' (right : ms) selection
monitorNext' _ selection = selection

modePrevious :: State -> State
modePrevious (State monitors (Selection monitor mode)) = State monitors (Selection monitor previous)
  where
    previous = modePrevious' mode available
    available = Types.available (monitorByName monitor monitors)

modePrevious' :: Text.Text -> [Text.Text] -> Text.Text
modePrevious' mode (top : next : rest)
  | top == mode = mode
  | next == mode = top
  | otherwise = modePrevious' mode (next : rest)
modePrevious' mode _ = mode

modeNext :: State -> State
modeNext (State monitors (Selection monitor mode)) = State monitors (Selection monitor next)
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

handleEvent :: T.BrickEvent Name e -> T.EventM Name State ()
handleEvent (T.VtyEvent e) = case e of
  Vty.EvKey Vty.KEsc [] -> M.halt
  Vty.EvKey (Vty.KChar 'q') [] -> M.halt
  Vty.EvKey Vty.KLeft [] -> T.modify monitorPrevious
  Vty.EvKey Vty.KRight [] -> T.modify monitorNext
  Vty.EvKey Vty.KUp [] -> T.modify modePrevious
  Vty.EvKey Vty.KDown [] -> T.modify modeNext
  Vty.EvKey Vty.KEnter [] -> changeMode
  Vty.EvKey (Vty.KChar ' ') [] -> changeMode
  _ -> pure ()
handleEvent _ = pure ()

changeMode :: T.EventM Name State ()
changeMode = do
  s <- T.get
  Monad.liftIO $ Hyprland.change (selectedMonitor $ sSelected s) (selectedMode $ sSelected s)

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
