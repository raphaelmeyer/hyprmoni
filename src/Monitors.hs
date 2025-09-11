{-# LANGUAGE OverloadedStrings #-}

module Monitors (info) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Json
import qualified Monitor

info :: Json.Json -> [Monitor.Info]
info (Json.Array monitors) = map sortModes . Maybe.mapMaybe monitorInfo $ monitors
info _ = []

monitorInfo :: Json.Json -> Maybe Monitor.Info
monitorInfo (Json.Object details) = do
  n <- monitorName details
  m <- currentMode details
  let as = availableModes details
  pure $ Monitor.Info n m as
monitorInfo _ = Nothing

monitorName :: Json.KeyValues -> Maybe Text.Text
monitorName details = do
  value <- Map.lookup "name" details
  case value of
    (Json.String n) -> pure n
    _ -> Nothing

currentMode :: Json.KeyValues -> Maybe Monitor.Mode
currentMode details = do
  width <- Map.lookup "width" details
  height <- Map.lookup "height" details
  case (width, height) of
    (Json.Number w, Json.Number h) -> pure $ Monitor.Mode (truncate w) (truncate h)
    _ -> Nothing

availableModes :: Json.KeyValues -> [Monitor.Mode]
availableModes details = case Map.lookup "availableModes" details of
  Just (Json.Array modes) -> List.nub $ Maybe.mapMaybe availableMode modes
  _ -> []

availableMode :: Json.Json -> Maybe Monitor.Mode
availableMode (Json.String m) = case Text.split (== '@') m of
  (wxh : _) ->
    if Text.null wxh
      then Nothing
      else case map (read . Text.unpack) $ Text.split (== 'x') wxh of
        [w, h] -> Just $ Monitor.Mode w h
        _ -> Nothing
  _ -> Nothing
availableMode _ = Nothing

sortModes :: Monitor.Info -> Monitor.Info
sortModes monitor = monitor {Monitor.available = sorted}
  where
    sorted = List.sortBy (flip compare) $ Monitor.available monitor
