{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import LadderLogic.Parser
import LadderLogic.Types

import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success x) = Just x
maybeSuccess _ = Nothing

testParse :: Parser a -> String -> Maybe a
testParse p s = let m = parseString p mempty s
                        in maybeSuccess m

-- Used in a comment parser test below
multilineComment :: String
multilineComment = [r|
!! This is a multiline comment !!
!! It always needs to be bookended by !!
|]

unrelatedRungs :: String
unrelatedRungs = [r|

!! Here are some comments that should be ignored !!
##----[/A]----(B)--------##
##-[C]---------------(D)-##

|]

diagramWithOring :: String
diagramWithOring = [r|
!! Comment !!
##----[A]---+--[B]--[C]--+---(D)--##
##          |            |        ##
##          +----[E]-----+        ##
|]

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
        testParse parseComments "!! Hello world !!" `shouldBe` Just ([" Hello world "])

      it "skips comments and parses input" $ do
        testParse (parseComments >> parseInput) "!! Hello world !![Hello]" `shouldBe` Just (Input "Hello")
      
      it "fails to skip invalid comments" $ do
        testParse (parseComments >> parseInput) "## This fails ## " `shouldBe` Nothing

      it "parses multiline comments" $ do
        let expected = Just ([" This is a multiline comment ", " It always needs to be bookended by "])
            actual = testParse parseComments multilineComment
        actual `shouldBe` expected
    
    describe "Ladder parser" $ do
      it "parses a simple ladder diagram" $ do
        let expected = Just ([And (Not (Input "A")) (Output "B")])
            actual = testParse parseLadder "##---[/A]---(B)---##"
        actual `shouldBe` expected

      it "parses a ladder diagram with unrelated rungs" $ do
        let expected = Just ([And (Not (Input "A")) (Output "B"),
                              And (Input "C") (Output "D")])
            actual = testParse parseLadder unrelatedRungs
        actual `shouldBe` expected

      it "parses a ladder with ORing wires" $ do
        let bc = And (Input "B") (Input "C")
            ebc = Or (Input "E") bc
            aebc = And (Input "A") ebc
            expected = Just [And aebc (Output "D")]
            actual = testParse parseLadder diagramWithOring
        actual `shouldBe` expected