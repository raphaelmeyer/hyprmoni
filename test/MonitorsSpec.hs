{-# LANGUAGE OverloadedStrings #-}

module MonitorsSpec where

import qualified Data.Map.Strict as Map
import qualified Json
import qualified Monitors
import Test.Hspec
import qualified Types

someModes :: Json.Json
someModes =
  Json.Array
    [ Json.String "1600x900@75",
      Json.String "1024x768@12.3",
      Json.String "1600x900@60",
      Json.String "1024x768@60.0",
      Json.String "1234x567@77",
      Json.String "1600x1200@12.5",
      Json.String "1500x1100@33.3"
    ]

singleMode :: Json.Json
singleMode = Json.Array [Json.String "1600x900"]

missingField :: Json.Json
missingField =
  Json.Object $
    Map.fromList
      [ ("name", Json.String "has no other required fields")
      ]

hdmi :: Json.Json
hdmi =
  Json.Object $
    Map.fromList
      [ ("name", Json.String "HDMI-1"),
        ("width", Json.Number 1600),
        ("height", Json.Number 900),
        ("availableModes", singleMode)
      ]

hdmi2 :: Json.Json
hdmi2 =
  Json.Object $
    Map.fromList
      [ ("name", Json.String "HDMI-A-2"),
        ("width", Json.Number 1600),
        ("height", Json.Number 1200),
        ("availableModes", someModes)
      ]

dp :: Json.Json
dp =
  Json.Object $
    Map.fromList
      [ ("name", Json.String "eDP-1"),
        ("width", Json.Number 1600),
        ("height", Json.Number 900),
        ("availableModes", someModes)
      ]

spec :: Spec
spec = do
  describe "no monitor" $ do
    it "should return an empty list" $ do
      let info = Monitors.info (Json.Array [])
      info `shouldBe` []

  describe "info" $ do
    it "should have one entry per monitor" $ do
      let one = Monitors.info (Json.Array [hdmi])
      length one `shouldBe` 1

      let two = Monitors.info (Json.Array [dp, hdmi])
      length two `shouldBe` 2

    it "should contain info for each monitor" $ do
      let names = map Types.name $ Monitors.info (Json.Array [dp, hdmi, hdmi2])
      names `shouldContain` ["HDMI-1"]
      names `shouldContain` ["HDMI-A-2"]
      names `shouldContain` ["eDP-1"]

    it "should return monitor info" $ do
      let info = head $ Monitors.info (Json.Array [hdmi])
      Types.name info `shouldBe` "HDMI-1"
      Types.mode info `shouldBe` "1600x900"
      Types.available info `shouldBe` ["1600x900"]

  describe "not well defined monitor" $ do
    it "should not return info if a field is missing" $ do
      let info = Monitors.info (Json.Array [missingField])
      info `shouldBe` []

    it "should return any other well defined monitor" $ do
      let info = Monitors.info (Json.Array [missingField, hdmi])
      length info `shouldBe` 1
      Types.name (head info) `shouldBe` "HDMI-1"

  describe "modes" $ do
    it "should contain list of all available modes" $ do
      let info = Types.available . head . Monitors.info $ Json.Array [dp]
      info `shouldContain` ["1024x768"]
      info `shouldContain` ["1234x567"]
      info `shouldContain` ["1500x1100"]
      info `shouldContain` ["1600x900"]
      info `shouldContain` ["1600x1200"]

    it "should contain each resolution only once, ignoring refresh rates" $ do
      let info = Types.available . head . Monitors.info $ Json.Array [dp]
      length info `shouldBe` 5

    it "should sort modes by width then by height in descending order" $ do
      let info = Types.available . head . Monitors.info $ Json.Array [dp]
      info `shouldBe` ["1600x1200", "1600x900", "1500x1100", "1234x567", "1024x768"]
