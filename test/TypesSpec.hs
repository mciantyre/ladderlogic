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

    it "matches inner flipped ANDs" $ do
      let ab = And (Input "A") (Input "B")
          ba = And (Input "B") (Input "A")
          abba = And ab ba
          baab = And ba ab
      And abba baab `shouldBe` And baab abba

  describe "Or" $ do
    it "matches itself" $ do
      Or (Input "A") (Output "B") `shouldBe` Or (Input "A") (Output "B")
    
    it "matches the flip of itself" $ do
      Or (Input "A") (Output "B") `shouldBe` Or (Output "B") (Input "A")

    it "fails to match for wrong inner logics" $ do
      Or (Input "A") (Input "B") `shouldNotBe` Or (Input "A") (Output "B")

    it "fails to match for wrong and flipped inner logics" $ do
      Or (Input "B") (Input "A") `shouldNotBe` Or (Input "A") (Output "B")

    it "matches inner flipped ORs" $ do
      let ab = Or (Input "A") (Input "B")
          ba = Or (Input "B") (Input "A")
          abba = Or ab ba
          baab = Or ba ab
      Or abba baab `shouldBe` Or baab abba

  describe "Simplification" $ do
    it "simplifies NOT" $ do
      simplify (Not NoOp) `shouldBe` NoOp

    it "does nothing to an input" $ do
      simplify (Input "A") `shouldBe` Input "A"

    it "does nothing to an output" $ do
      simplify (Output "B") `shouldBe` Output "B"

    it "does nothing for a normal NOT" $ do
      simplify (Not (Input "A")) `shouldBe` Not (Input "A")

    it "simplifies an AND" $ do
      simplify (And NoOp (Input "A")) `shouldBe` Input "A"
      simplify (And (Input "A") NoOp) `shouldBe` Input "A"

    it "simplifies an AND through a NOT" $ do
      simplify (Not (And NoOp (Input "A"))) `shouldBe` Not (Input "A")
  
    it "simplifies an OR" $ do
      simplify (Or NoOp (Input "A")) `shouldBe` Input "A"
      simplify (Or (Input "A") NoOp) `shouldBe` Input "A"

    it "simplifies nested binary logics" $ do
      simplify (And NoOp (Or (Output "Z") NoOp)) `shouldBe` Output "Z"
      let orab = Or (Input "A") (Input "B")
      simplify (And NoOp orab) `shouldBe` orab

    it "simplifies binary logics of two NoOps" $ do
      simplify (And NoOp NoOp) `shouldBe` NoOp
      simplify (Or NoOp NoOp) `shouldBe` NoOp

    it "simplifies a fold with AND" $ do
      let logics = [(Input "A"), (Input "B"), (Input "C"), (Input "D")]
          actual = simplify $ foldl And NoOp logics
          expected = foldl And (head logics) (tail logics)
      actual `shouldBe` expected

    it "simplifies a fold with OR" $ do
      let logics = [(Input "A"), (Input "B"), (Input "C"), (Input "D")]
          actual = simplify $ foldl Or NoOp logics
          expected = foldl Or (head logics) (tail logics)
      actual `shouldBe` expected
    