{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Control.Exception as Exception
import qualified Data.Text as Text

data MonitorInfo = MonitorInfo
  { name :: Text.Text,
    mode :: Text.Text,
    available :: [Text.Text]
  }
  deriving (Eq, Show)

data ApplicationException
  = UndefinedEnvironmentVariable Text.Text
  | SocketNotFound
  | InvalidJson

instance Show ApplicationException where
  show (UndefinedEnvironmentVariable variable) =
    Text.unpack $
      Text.concat ["Missing environment variable '", variable, "'"]
  show SocketNotFound = "Hyprland IPC socket not found"
  show InvalidJson = "Could not parse json"

instance Exception.Exception ApplicationException
