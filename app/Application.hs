{-# LANGUAGE OverloadedStrings #-}

module Application (run) where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Control.Monad.IO.Class as Monad
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import qualified Hyprland
import qualified Types
import qualified UI

mkApp :: M.App UI.State e UI.Name
mkApp =
  M.App
    { M.appDraw = UI.draw,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = pure (),
      M.appAttrMap = const UI.attributes
    }

run :: [Types.MonitorInfo] -> IO ()
run monitors = do
  _ <- M.defaultMain mkApp (initialState monitors)
  pure ()

initialState :: [Types.MonitorInfo] -> UI.State
initialState monitors = UI.State monitors (UI.Selection (Types.name . head $ monitors) (Types.mode . head $ monitors))

monitorPrevious :: UI.State -> UI.State
monitorPrevious (UI.State monitors selection) = UI.State monitors (monitorPrevious' monitors selection)

monitorPrevious' :: [Types.MonitorInfo] -> UI.Selection -> UI.Selection
monitorPrevious' (left : right : ms) selection
  | Types.name left == UI.selectedMonitor selection = selection
  | Types.name right == UI.selectedMonitor selection = UI.Selection (Types.name left) (Types.mode left)
  | otherwise = monitorPrevious' (right : ms) selection
monitorPrevious' _ selection = selection

monitorNext :: UI.State -> UI.State
monitorNext (UI.State monitors selection) = UI.State monitors (monitorNext' monitors selection)

monitorNext' :: [Types.MonitorInfo] -> UI.Selection -> UI.Selection
monitorNext' (left : right : ms) selection
  | Types.name left == UI.selectedMonitor selection = UI.Selection (Types.name right) (Types.mode right)
  | otherwise = monitorNext' (right : ms) selection
monitorNext' _ selection = selection

modePrevious :: UI.State -> UI.State
modePrevious (UI.State monitors (UI.Selection monitor mode)) = UI.State monitors (UI.Selection monitor previous)
  where
    previous = modePrevious' mode available
    available = Types.available (monitorByName monitor monitors)

modePrevious' :: Text.Text -> [Text.Text] -> Text.Text
modePrevious' mode (top : next : rest)
  | top == mode = mode
  | next == mode = top
  | otherwise = modePrevious' mode (next : rest)
modePrevious' mode _ = mode

modeNext :: UI.State -> UI.State
modeNext (UI.State monitors (UI.Selection monitor mode)) = UI.State monitors (UI.Selection monitor next)
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

handleEvent :: T.BrickEvent UI.Name e -> T.EventM UI.Name UI.State ()
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

changeMode :: T.EventM UI.Name UI.State ()
changeMode = do
  s <- T.get
  Monad.liftIO $ Hyprland.change (UI.selectedMonitor $ UI.sSelected s) (UI.selectedMode $ UI.sSelected s)
