{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import Text.LadderLogic.Types

import Test.Hspec

spec = describe "Types" $ do
  describe "Input" $ do
    it "matches for same inputs" $ do
      Input "A" `shouldBe` Input "A"

    it "fails to match for different inputs" $ do
      Input "A" `shouldNotBe` Input "B"

    it "fails to match wrong type" $ do
      Input "A" `shouldNotBe` Output "A"

    it "fails to match a NOT of itself" $ do
      Not (Input "A") `shouldNotBe` Input "A"

  describe "Output" $ do
    it "matches for same output" $ do
      Output "A" `shouldBe` Output "A"

    it "fails to match for different outputs" $ do
      Output "A" `shouldNotBe` Output "B"

    it "fails to match a NOT of itself" $ do
      Not (Output "A") `shouldNotBe` Output "A"

  describe "Not" $ do
    it "matches itself" $ do
      Not (Input "A") `shouldBe` Not (Input "A")

    it "fails to match for wrong types" $ do
      Not (Input "A") `shouldNotBe` Not (Output "A")

    it "fails to match for wrong values" $ do
      Not (Input "A") `shouldNotBe` Not (Input "B")

  describe "And" $ do
    it "matches itself" $ do
      And (Input "A") (Output "B") `shouldBe` And (Input "A") (Output "B")
    
    it "matches the flip of itself" $ do
      And (Input "A") (Output "B") `shouldBe` And (Output "B") (Input "A")

    it "fails to match for wrong inner logics" $ do
      And (Input "A") (Input "B") `shouldNotBe` And (Input "A") (Output "B")

    it "fails to match for wrong and flipped inner logics" $ do
      And (Input "B") (Input "A") `shouldNotBe` And (Input "A") (Output "B")

  describe "Or" $ do
    it "matches itself" $ do
      Or (Input "A") (Output "B") `shouldBe` Or (Input "A") (Output "B")
    
    it "matches the flip of itself" $ do
      Or (Input "A") (Output "B") `shouldBe` Or (Output "B") (Input "A")

    it "fails to match for wrong inner logics" $ do
      Or (Input "A") (Input "B") `shouldNotBe` Or (Input "A") (Output "B")

    it "fails to match for wrong and flipped inner logics" $ do
      Or (Input "B") (Input "A") `shouldNotBe` Or (Input "A") (Output "B")