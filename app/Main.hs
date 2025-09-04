module Main where

import qualified Application
import qualified Control.Exception as Exception
import qualified System.Exit as System

main :: IO ()
main = do
  Exception.handle onError $ do
    Application.run

onError :: Exception.SomeException -> IO a
onError e = do
  print e
  System.exitFailure
