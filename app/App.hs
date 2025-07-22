module App (run) where

import qualified Brick.AttrMap as Attr
import qualified Brick.Main as M
import qualified Brick.Types as T
-- import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Core as Core
import qualified Graphics.Vty as Vty

data State = State

data Name = Name deriving (Eq, Ord)

mkApp :: M.App State e Name
mkApp =
  M.App
    { M.appDraw = draw,
      M.appChooseCursor = M.showFirstCursor,
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
draw _ = [Core.vBox [Core.str "hyprmoni"]]

handleEvent :: T.BrickEvent Name e -> T.EventM Name State ()
handleEvent (T.VtyEvent e) = case e of
  Vty.EvKey Vty.KEsc [] -> M.halt
  Vty.EvKey (Vty.KChar 'q') [] -> M.halt
  _ -> pure ()
handleEvent _ = pure ()

attributes :: Attr.AttrMap
attributes = Attr.attrMap Vty.defAttr []
