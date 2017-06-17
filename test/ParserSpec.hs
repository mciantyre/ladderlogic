{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import Text.LadderLogic.Parser
import Text.LadderLogic.Types

import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success x) = Just x
maybeSuccess _ = Nothing

testMaybe :: Parser a -> String -> Maybe a
testMaybe p s = let m = parseString p mempty s
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
##----[/A]----(B)--------------------------------##
##-[C]---------------------------------------(D)-##

|]

diagramWithOring :: String
diagramWithOring = [r|
!! Comment                        !!
##----[A]---+--[B]--[C]--+---(D)--##
##          |            |        ##
##          +----[E]-----+        ##
|]

multipleRungs :: String
multipleRungs = [r|
!!   A multi-rung program          !!
##---[/A]--+---[B]---------+--(D)--##
##         |               |       ##
##         +----[/C]-------+       ##
##                                 ##
##------------[/F]-------(G)-------##
|]

multipleOrs :: String
multipleOrs = [r|
!! A program with multiple OR wires !!
##---[A]---+-------[B]-----+---(D)--##
##         |               |        ##
##         +-------[C]-----+        ##
##         |               |        ##
##         +-------[E]-----+        ##
|]

spec =
  describe "Parser" $ do
    describe "Input parser" $ do
      it "parses input name from brackets" $ do
        (Just (Input "INPUT")) `shouldBe` (testMaybe parseInput "[INPUT]")
      
      it "parses the NOT indicator ('/') from brackets" $ do
        let expected = Just (Not (Input "INPUT"))
            actual = testMaybe parseInput "[/INPUT]"
        actual `shouldBe` expected

      it "does not parse empty identifiers" $ do
        Nothing `shouldBe` testMaybe parseInput "[/]"

      it "parses numbers in identifiers" $ do
        testMaybe parseInput "[1234567]" `shouldBe` Just (Input "1234567")
      
      it "does not parse letters and numbers in identifiers" $ do
        testMaybe parseInput "[ABC123]" `shouldBe` Just (Input "ABC123")

      it "does not parse spaces in identifiers" $ do
        testMaybe parseInput "[ABC DEF]" `shouldBe` Nothing

      it "parses - (dash) as a delimiter" $ do
        Just (Not (Input "AB-1c-5d")) `shouldBe` testMaybe parseInput "[/AB-1c-5d]"

      it "parses _ (underscore) as a delimiter" $ do
        testMaybe parseInput "[/AB_1c_5d]" `shouldBe` Just (Not (Input "AB_1c_5d"))

      it "does not parse a valid output" $ do
        testMaybe parseInput "(OUTPUT)" `shouldBe` Nothing

    describe "Output parser" $ do
      it "parses the output name from parentheses" $ do
        let expected = Just (Output "OUTPUT")
            actual = testMaybe parseOutput "(OUTPUT)"
        actual `shouldBe` expected

      it "does not parse empty identifiers" $ do
        Nothing `shouldBe` testMaybe parseOutput "()"

      it "parses numbers in identifiers" $ do
        testMaybe parseOutput "(1234567)" `shouldBe` Just (Output "1234567")

      it "does not parse letters and numbers in identifiers" $ do
        testMaybe parseOutput "(ABC123)" `shouldBe` Just (Output "ABC123")

      it "does not parse spaces in identifiers" $ do
        Nothing `shouldBe` testMaybe parseOutput "(ABC DEF)"

      it "parses - (dash) as a delimiter" $ do
        testMaybe parseOutput "(AB-1c-5d)" `shouldBe` Just (Output "AB-1c-5d")
      
      it "parses _ (underscore) as a delimiter" $ do
        testMaybe parseOutput "(AB_1c_5d)" `shouldBe` Just (Output "AB_1c_5d")

      it "does not parse a valid input" $ do
        testMaybe parseOutput "[INPUT]" `shouldBe` Nothing

    describe "Comment parser" $ do
      it "returns the contents of the comment" $ do
        testMaybe parseComments "!! Hello world !!" `shouldBe` Just ([" Hello world "])

      it "skips comments and parses input" $ do
        testMaybe (parseComments >> parseInput) "!! Hello world !![Hello]" `shouldBe` Just (Input "Hello")
      
      it "fails to skip invalid comments" $ do
        testMaybe (parseComments >> parseInput) "## This fails ## " `shouldBe` Nothing

      it "parses multiline comments" $ do
        let expected = Just ([" This is a multiline comment ", " It always needs to be bookended by "])
            actual = testMaybe parseComments multilineComment
        actual `shouldBe` expected
    
    describe "Ladder parser" $ do
      it "parses a simple ladder diagram" $ do
        let expected = Just ([And (Not (Input "A")) (Output "B")])
            actual = testMaybe parseLadder "##---[/A]---(B)---##"
        actual `shouldBe` expected

      it "parses a valid diagram with +s in the middle" $ do
        let ladder = "##--[A]---+--[B]--++---[C]---+----(D)--##"
            ab = And (Input "A") (Input "B")
            abc = And ab (Input "C")
            expceted = Just [And abc (Output "D")]
            actual = testMaybe parseLadder ladder
        actual `shouldBe` expceted

      it "parses a ladder diagram with unrelated rungs" $ do
        let expected = Just ([And (Not (Input "A")) (Output "B"),
                              And (Input "C") (Output "D")])
            actual = testMaybe parseLadder unrelatedRungs
        actual `shouldBe` expected

      it "parses a ladder with ORing wires" $ do
        let bc = And (Input "B") (Input "C")
            ebc = Or (Input "E") bc
            aebc = And (Input "A") ebc
            expected = Just [And aebc (Output "D")]
            actual = testMaybe parseLadder diagramWithOring
        actual `shouldBe` expected

      it "parses a multi-rung ladder program" $ do
        let bc = Or (Not (Input "C")) (Input "B")
            abc = And (Not (Input "A")) bc
            rung1 = And (abc) (Output "D")
            rung2 = And (Not (Input "F")) (Output "G")
            expected = Just [rung1, rung2]
            actual = testMaybe parseLadder multipleRungs
        actual `shouldBe` expected

      it "parses a ladder with multiple OR wires" $ do
        let ec = Or (Input "E") (Input "C")
            bce = Or ec (Input "B")
            abce = And (Input "A") bce
            expected = Just [And abce (Output "D")]
            actual = testMaybe parseLadder multipleOrs
        actual `shouldBe` expected
