{-# LANGUAGE OverloadedStrings #-}

module SelectionSpec where

import qualified Monitor
import qualified Selection
import Test.Hspec

hdplus :: Monitor.Mode
hdplus = Monitor.Mode 1600 900

uxga :: Monitor.Mode
uxga = Monitor.Mode 1600 1200

fhd :: Monitor.Mode
fhd = Monitor.Mode 1920 1080

wuxga :: Monitor.Mode
wuxga = Monitor.Mode 1920 1200

uhd4k :: Monitor.Mode
uhd4k = Monitor.Mode 3840 2160

hdmi :: Monitor.Info
hdmi = Monitor.Info "HDMI-1" fhd [fhd]

hdmi2 :: Monitor.Info
hdmi2 = Monitor.Info "HDMI-2" fhd [fhd]

dp :: Monitor.Info
dp = Monitor.Info "eDP-1" fhd [fhd]

spec :: Spec
spec = do
  describe "first monitor" $ do
    it "should select the monitor if there is just one" $ do
      let selection = Selection.first [hdmi {Monitor.name = "Foo"}]
      Selection.selectedMonitor selection `shouldBe` "Foo"

    it "should select the mode if there is just one" $ do
      let selection = Selection.first [hdmi {Monitor.mode = uxga, Monitor.available = [uxga]}]
      Selection.selectedMode selection `shouldBe` uxga

    it "should select the current mode" $ do
      let selection = Selection.first [hdmi {Monitor.mode = fhd, Monitor.available = [wuxga, fhd]}]
      Selection.selectedMode selection `shouldBe` fhd

  -- it "should ..." $ do
  --   let selection = Selection.first []
  --   Selection.selectedMode selection `shouldBe` "???"

  -- it "shoud ..." $ do
  --   let selection = Selection.first [dp {Monitor.mode = "1024x786", Monitor.available = ["1600x1200"]}]
  --   Selection.selectedMode selection `shouldBe` "???"

  -- it "shoud ..." $ do
  --   let selection = Selection.first [dp {Monitor.mode = "1024x786", Monitor.available = []}]
  --   Selection.selectedMode selection `shouldBe` "???"

  -- describe "next mode" $ do

  -- describe "previous mode" $ do

  describe "next monitor" $ do
    it "should keep the monitor selected if there is just one" $ do
      let monitors = [hdmi {Monitor.name = "Selected"}]
      let next = Selection.nextMonitor monitors (Selection.first monitors)
      Selection.selectedMonitor next `shouldBe` "Selected"

    it "should select the next monitor" $ do
      let monitors = [hdmi {Monitor.name = "Selected"}, hdmi2 {Monitor.name = "Next"}]
      let next = Selection.nextMonitor monitors (Selection.first monitors)
      Selection.selectedMonitor next `shouldBe` "Next"

    it "should keep the last monitor selected" $ do
      let monitors = [hdmi {Monitor.name = "Selected"}, hdmi2 {Monitor.name = "Next"}]
      let next = Selection.nextMonitor monitors . Selection.nextMonitor monitors $ (Selection.first monitors)
      Selection.selectedMonitor next `shouldBe` "Next"

    it "should select the current mode of the newly selected monitor" $ do
      let monitors =
            [ hdmi2,
              dp {Monitor.mode = hdplus, Monitor.available = [uhd4k, wuxga, fhd, hdplus]}
            ]
      let next = Selection.nextMonitor monitors (Selection.first monitors)
      Selection.selectedMode next `shouldBe` hdplus

    it "should keep the selected mode if the selected monitor does not change" $ do
      let monitors = [hdmi2 {Monitor.mode = wuxga, Monitor.available = [wuxga, uxga, hdplus]}]
      let next = Selection.nextMonitor monitors . Selection.nextMode monitors $ Selection.first monitors
      Selection.selectedMode next `shouldBe` uxga

-- it "should keep the selected mode if the selected monitor does not change"
--   let monitors = [hdmi2 { ... }, dp { ... } ]

-- it "should ..." $ do
--   let monitors =
--         [ hdmi2,
--           dp {Monitor.mode = "1200x900", Monitor.available = ["1600x1200", "1024x768"]}
--         ]
--   let next = Selection.nextMonitor monitors (Selection.first monitors)
--   Selection.selectedMode next `shouldBe` "???"

-- it "should ..." $ do
--   Selection.nextMonitor [] selection"
