{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import LadderLogic.Parser
import LadderLogic.Types

import Test.Hspec
import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success x) = Just x
maybeSuccess _ = Nothing

testParse :: Parser a -> String -> Maybe a
testParse p s = let m = parseString p mempty s
                        in maybeSuccess m

spec =
  describe "Parser" $ do
    describe "Input parser" $ do
      it "parses input name from brackets" $ do
        (Just (Input "INPUT")) `shouldBe` (testParse parseInput "[INPUT]")
      
      it "parses the NOT indicator ('/') from brackets" $ do
        let expected = Just (Not (Input "INPUT"))
            actual = testParse parseInput "[/INPUT]"
        actual `shouldBe` expected

      it "does not parse empty identifiers" $ do
        Nothing `shouldBe` testParse parseInput "[/]"

      it "parses numbers in identifiers" $ do
        testParse parseInput "[1234567]" `shouldBe` Just (Input "1234567")
      
      it "does not parse letters and numbers in identifiers" $ do
        testParse parseInput "[ABC123]" `shouldBe` Just (Input "ABC123")

      it "does not parse spaces in identifiers" $ do
        testParse parseInput "[ABC DEF]" `shouldBe` Nothing

      it "parses - (dash) as a delimiter" $ do
        Just (Not (Input "AB-1c-5d")) `shouldBe` testParse parseInput "[/AB-1c-5d]"

      it "parses _ (underscore) as a delimiter" $ do
        testParse parseInput "[/AB_1c_5d]" `shouldBe` Just (Not (Input "AB_1c_5d"))

      it "does not parse a valid output" $ do
        testParse parseInput "(OUTPUT)" `shouldBe` Nothing

    describe "Output parser" $ do
      it "parses the output name from parentheses" $ do
        let expected = Just (Output "OUTPUT")
            actual = testParse parseOutput "(OUTPUT)"
        actual `shouldBe` expected

      it "does not parse empty identifiers" $ do
        Nothing `shouldBe` testParse parseOutput "()"

      it "parses numbers in identifiers" $ do
        testParse parseOutput "(1234567)" `shouldBe` Just (Output "1234567")

      it "does not parse letters and numbers in identifiers" $ do
        testParse parseOutput "(ABC123)" `shouldBe` Just (Output "ABC123")

      it "does not parse spaces in identifiers" $ do
        Nothing `shouldBe` testParse parseOutput "(ABC DEF)"

      it "parses - (dash) as a delimiter" $ do
        testParse parseOutput "(AB-1c-5d)" `shouldBe` Just (Output "AB-1c-5d")
      
      it "parses _ (underscore) as a delimiter" $ do
        testParse parseOutput "(AB_1c_5d)" `shouldBe` Just (Output "AB_1c_5d")

      it "does not parse a valid input" $ do
        testParse parseOutput "[INPUT]" `shouldBe` Nothing

    describe "Comment parser" $ do
      it "returns the contents of the comment" $ do
        testParse skipComments "!! Hello world !!" `shouldBe` Just (" Hello world ")

      it "skips comments and parses input" $ do
        testParse (skipComments >> parseInput) "!! Hello world !![Hello]" `shouldBe` Just (Input "Hello")
      
      it "fails to skip invalid comments" $ do
        testParse (skipComments >> parseInput) "## This fails ##[Hello]" `shouldBe` Nothing

    describe "Rung parser" $ do
      it "parsers a rung of one input" $ do
        let rung = "||--------[INPUT]----||"
            actual = testParse parseRung rung 
        actual `shouldBe` Just (Input "INPUT")

      it "parses a rung of one input and one output" $ do
        let rung = "||--[/INPUT]---(OUTPUT)---||"
        testParse parseRung rung `shouldBe` Just (And (Not (Input "INPUT")) (Output "OUTPUT"))

      it "parses a rung of two inputs" $ do
        let rung = "||----[/A]----[B]---||"
            actual = testParse parseRung rung
        actual `shouldBe` Just (And (Not (Input "A")) (Input "B"))

      it "parses a rung of three inputs" $ do
        let rung = "||--[A]--[B]---------------[/C]-||"
            actual = testParse parseRung rung
            ab = And (Input "A") (Input "B")
            c = Not (Input "C")
            expected = Just (And ab c)
        actual `shouldBe` expected

      it "parses a run of three inputs and one output" $ do
        let rung = "||--[A]-[/B]--------[C]-------------------------(D)-----||"
            actual = testParse parseRung rung
            ab = And (Input "A") (Not (Input "B"))
            abc = And ab (Input "C")
            abcd = And abc (Output "D")
            expected = Just abcd
        actual `shouldBe` expected

      it "fails to parse a rung of no inputs or outputs" $ do
        let rung = "||-----------||"
        testParse parseRung rung `shouldBe` Nothing
      
      it "fails to parse an invalid rung" $ do
        let rung = "|----[A]---||" -- missing first '|'
        testParse parseRung rung `shouldBe` Nothing

      it "fails to parse just wires" $ do
        let rung = "----[A]---(B)----" -- missing first '|'
        testParse parseRung rung `shouldBe` Nothing

      it "parses juxtaposed elements" $ do
        let rung = "||----[A](B)---||"
        testParse parseRung rung `shouldBe` Just (And (Input "A") (Output "B"))

      it "fails to parse elements next to the rung delimiter" $ do
        let rung = "||[/A]-(B)||"
        testParse parseRung rung `shouldBe` Just (And (Not (Input "A")) (Output "B"))

      it "parses empty rungs" $ do
        let rung = "||                    ||"
        testParse parseEmptyRung rung `shouldBe` Just ()

      it "parses empty rungs with vertical connectors" $ do
        let rung = "||     |    | |  ||"
        testParse parseEmptyRung rung `shouldBe` Just ()