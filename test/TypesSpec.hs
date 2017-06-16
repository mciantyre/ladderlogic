{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import LadderLogic.Types

import Test.Hspec

shouldBeTrue b = b `shouldBe` True
shouldBeFalse b = b `shouldBe` False

spec = describe "Types" $ do
    describe "Input" $ do
        it "matches for same inputs" $ do
            shouldBeTrue $ (Input "A") == (Input "A")

        it "fails to match for different inputs" $ do
            shouldBeFalse $ (Input "A") == (Input "B")
