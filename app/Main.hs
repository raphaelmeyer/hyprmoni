module Main where

import qualified App
import qualified Control.Exception as Exception
import qualified Hyprland
import qualified System.Exit as System

main :: IO ()
main = do
  Exception.handle onError $ do
    monitors <- Hyprland.allMonitors
    print monitors
    App.run monitors

onError :: Exception.SomeException -> IO a
onError e = do
  print e
  System.exitFailure
