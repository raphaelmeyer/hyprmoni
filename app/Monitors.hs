{-# LANGUAGE OverloadedStrings #-}

module Monitors where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Json
import qualified Text.Printf as Printf

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
  m <- currentMode details
  let as = availableModes details
  pure $ Info n m as
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
