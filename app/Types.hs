{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Control.Exception as Exception
import qualified Data.Text as Text

data MonitorInfo = MonitorInfo
  { name :: Text.Text,
    mode :: Text.Text,
    available :: [Text.Text]
  }
  deriving (Show)

data ApplicationException = UndefinedEnvironmentVariable Text.Text | SocketNotFound

instance Show ApplicationException where
  show (UndefinedEnvironmentVariable variable) =
    Text.unpack $
      Text.concat ["Missing environment variable '", variable, "'"]
  show SocketNotFound = "Hyprland IPC socket not found"

instance Exception.Exception ApplicationException
