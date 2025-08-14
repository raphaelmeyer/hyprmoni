{-# LANGUAGE OverloadedStrings #-}

module Application (run) where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Control.Monad.IO.Class as Monad
import qualified Graphics.Vty as Vty
import qualified Hyprland
import qualified Selection
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
initialState monitors = UI.State monitors (Selection.Selection (Types.name . head $ monitors) (Types.mode . head $ monitors))

monitorPrevious :: UI.State -> UI.State
monitorPrevious (UI.State monitors selection) = UI.State monitors (Selection.previousMonitor monitors selection)

monitorNext :: UI.State -> UI.State
monitorNext (UI.State monitors selection) = UI.State monitors (Selection.nextMonitor monitors selection)

modePrevious :: UI.State -> UI.State
modePrevious (UI.State monitors selection) = UI.State monitors (Selection.previousMode monitors selection)

modeNext :: UI.State -> UI.State
modeNext (UI.State monitors selection) = UI.State monitors (Selection.nextMode monitors selection)

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
  Monad.liftIO $ Hyprland.change (Selection.selectedMonitor $ UI.sSelected s) (Selection.selectedMode $ UI.sSelected s)
