{-# LANGUAGE OverloadedStrings #-}

module Monitors where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Json

data Info = Info
  { name :: Text.Text,
    mode :: Text.Text,
    available :: [Text.Text]
  }
  deriving (Show)

info :: Json.Json -> [Info]
info (Json.Array monitors) = Maybe.mapMaybe monitorInfo monitors
info _ = []

monitorInfo :: Json.Json -> Maybe Info
monitorInfo (Json.Object details) = do
  n <- monitorName details
  pure $ Info n Text.empty []
monitorInfo _ = Nothing

monitorName :: Json.KeyValues -> Maybe Text.Text
monitorName details = do
  value <- Map.lookup "name" details
  case value of
    (Json.String n) -> pure n
    _ -> Nothing
