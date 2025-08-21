{-# LANGUAGE OverloadedStrings #-}

module JsonSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Json
import Test.Hspec

spec :: Spec
spec = do
  describe "empty root elements" $ do
    it "should return an empty object" $ do
      let json = Json.parse "{}"
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json `shouldBe` Json.Object Map.empty

    it "should return an empty array" $ do
      let json = Json.parse "[]"
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json `shouldBe` Json.Array []

  describe "parse error" $ do
    it "should return nothing if parsing fails" $ do
      let notJson = Json.parse "{ not valid ] }"
      notJson `shouldBe` Nothing

  describe "object" $ do
    it "should return the object and its values" $ do
      let json =
            Json.parse "[{ \"value\": 42, \"string\": \"a string\", \"flag\": true, \"values\": [1,2,3], \"nil\": null, \"object\": {}}]"
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json
        `shouldBe` Json.Array
          [ Json.Object $
              Map.fromList
                [ ("flag", Json.Boolean True),
                  ("nil", Json.Null),
                  ("object", Json.Object Map.empty),
                  ("string", Json.String "a string"),
                  ("value", Json.Number 42),
                  ("values", Json.Array [Json.Number 1, Json.Number 2, Json.Number 3])
                ]
          ]

  describe "array" $ do
    it "should allow arrays of items with different types" $ do
      let json = Json.parse "[\"foo\", 17, null]"
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json `shouldBe` Json.Array [Json.String "foo", Json.Number 17, Json.Null]
