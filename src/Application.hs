{-# LANGUAGE OverloadedStrings #-}

module Application (run) where

import qualified Brick.BChan as BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import qualified Hyprland
import qualified Selection
import qualified Types
import qualified UI

data Event = Update

mkApp :: M.App UI.State Event UI.Name
mkApp =
  M.App
    { M.appDraw = UI.draw,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = pure (),
      M.appAttrMap = const UI.attributes
    }

run :: IO ()
run = do
  evChan <- BChan.newBChan 16
  subscription <- Hyprland.subscribe $ onHyprlandEvent evChan
  monitors <- Hyprland.allMonitors
  (_, vty) <- M.customMainWithDefaultVty (Just evChan) mkApp (initialState monitors)
  Vty.shutdown vty
  Hyprland.unsubscribe subscription

onHyprlandEvent :: BChan.BChan Event -> Hyprland.Event -> IO ()
onHyprlandEvent evChan event = do
  Monad.when (isMonitorEvent event) $ BChan.writeBChan evChan Update

isMonitorEvent :: Hyprland.Event -> Bool
isMonitorEvent event = any startsWith ["monitoradded", "monitorremoved"]
  where
    startsWith prefix = Text.isPrefixOf prefix (Hyprland.evName event)

initialState :: [Types.MonitorInfo] -> UI.State
initialState monitors = UI.State monitors (Selection.first monitors)

monitorPrevious :: UI.State -> UI.State
monitorPrevious (UI.State monitors selection) = UI.State monitors (Selection.previousMonitor monitors selection)

monitorNext :: UI.State -> UI.State
monitorNext (UI.State monitors selection) = UI.State monitors (Selection.nextMonitor monitors selection)

modePrevious :: UI.State -> UI.State
modePrevious (UI.State monitors selection) = UI.State monitors (Selection.previousMode monitors selection)

modeNext :: UI.State -> UI.State
modeNext (UI.State monitors selection) = UI.State monitors (Selection.nextMode monitors selection)

handleEvent :: T.BrickEvent UI.Name Event -> T.EventM UI.Name UI.State ()
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
handleEvent (T.AppEvent ev) = case ev of
  Update -> updateMonitors
handleEvent _ = pure ()

changeMode :: T.EventM UI.Name UI.State ()
changeMode = do
  s <- T.get
  MonadIO.liftIO $ Hyprland.change (Selection.selectedMonitor $ UI.sSelected s) (Selection.selectedMode $ UI.sSelected s)

updateMonitors :: T.EventM UI.Name UI.State ()
updateMonitors = do
  s <- T.get
  monitors <- MonadIO.liftIO $ Hyprland.allMonitors
  T.put s {UI.sMonitors = monitors, UI.sSelected = Selection.first monitors}
