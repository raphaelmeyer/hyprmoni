{-# LANGUAGE OverloadedStrings #-}

module Monitors (info) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Json
import qualified Text.Printf as Printf
import qualified Types

info :: Json.Json -> [Types.MonitorInfo]
info (Json.Array monitors) = map sortModes . Maybe.mapMaybe monitorInfo $ monitors
info _ = []

monitorInfo :: Json.Json -> Maybe Types.MonitorInfo
monitorInfo (Json.Object details) = do
  n <- monitorName details
  m <- currentMode details
  let as = availableModes details
  pure $ Types.MonitorInfo n m as
monitorInfo _ = Nothing

monitorName :: Json.KeyValues -> Maybe Text.Text
monitorName details = do
  value <- Map.lookup "name" details
  case value of
    (Json.String n) -> pure n
    _ -> Nothing

currentMode :: Json.KeyValues -> Maybe Text.Text
currentMode details = do
  width <- Map.lookup "width" details
  height <- Map.lookup "height" details
  case (width, height) of
    (Json.Number w, Json.Number h) -> pure . Text.pack $ Printf.printf "%.gx%.g" w h
    _ -> Nothing

availableModes :: Json.KeyValues -> [Text.Text]
availableModes details = case Map.lookup "availableModes" details of
  Just (Json.Array modes) -> List.nub $ Maybe.mapMaybe availableMode modes
  _ -> []

availableMode :: Json.Json -> Maybe Text.Text
availableMode (Json.String m) = case Text.split (== '@') m of
  (wxh : _) -> if Text.null wxh then Nothing else Just wxh
  _ -> Nothing
availableMode _ = Nothing

sortModes :: Types.MonitorInfo -> Types.MonitorInfo
sortModes monitor = monitor {Types.available = sorted}
  where
    sorted = List.sortBy (flip compareMode) (Types.available monitor)

compareMode :: Text.Text -> Text.Text -> Ordering
compareMode a b = case compare (head dimA) (head dimB) of
  EQ -> compare (last dimA) (last dimB)
  result -> result
  where
    dimensions mode = map (read . Text.unpack) . Text.split (== 'x') $ mode :: [Int]
    dimA = dimensions a
    dimB = dimensions b
