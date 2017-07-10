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

simpleOrs :: String
simpleOrs = [r|
!! Simple oring !!
##---+--[A]--+---##
##   |       |   ##
##   +--[B]--+   ##
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

sequentialOrs :: String
sequentialOrs = [r|
!! A program with sequential OR wires !!
##---[A]--+--[B]--+-+---[C]---+---(D)-##
##        |       | |         |       ##
##        +--[E]--+ +---[F]---+       ##
|]

spanningOrs :: String
spanningOrs = [r|
!!     A program with spanning ORs     !!
##---[A]--+--[B]---+---[C]----+---(D)--##
##        |        |          |        ##
##        +--[E]---+          |        ##
##        |                   |        ##
##        +-------[F]---------+        ##
|]

spec =
  describe "Parser" $ do
    describe "Input parser" $ do
      it "parses input name from brackets" $ do
        (Just (Input "INPUT")) `shouldBe` (testMaybe input "[INPUT]")
      
      it "parses the NOT indicator ('/') from brackets" $ do
        let expected = Just (Not (Input "INPUT"))
            actual = testMaybe input "[/INPUT]"
        actual `shouldBe` expected

      it "does not parse empty identifiers" $ do
        Nothing `shouldBe` testMaybe input "[/]"

      it "parses numbers in identifiers" $ do
        testMaybe input "[1234567]" `shouldBe` Just (Input "1234567")
      
      it "does not parse letters and numbers in identifiers" $ do
        testMaybe input "[ABC123]" `shouldBe` Just (Input "ABC123")

      it "does not parse spaces in identifiers" $ do
        testMaybe input "[ABC DEF]" `shouldBe` Nothing

      it "parses - (dash) as a delimiter" $ do
        Just (Not (Input "AB-1c-5d")) `shouldBe` testMaybe input "[/AB-1c-5d]"

      it "parses _ (underscore) as a delimiter" $ do
        testMaybe input "[/AB_1c_5d]" `shouldBe` Just (Not (Input "AB_1c_5d"))

      it "does not parse a valid output" $ do
        testMaybe input "(OUTPUT)" `shouldBe` Nothing

    describe "Output parser" $ do
      it "parses the output name from parentheses" $ do
        let expected = Just (Output "OUTPUT")
            actual = testMaybe output "(OUTPUT)"
        actual `shouldBe` expected

      it "does not parse empty identifiers" $ do
        Nothing `shouldBe` testMaybe output "()"

      it "parses numbers in identifiers" $ do
        testMaybe output "(1234567)" `shouldBe` Just (Output "1234567")

      it "does not parse letters and numbers in identifiers" $ do
        testMaybe output "(ABC123)" `shouldBe` Just (Output "ABC123")

      it "does not parse spaces in identifiers" $ do
        Nothing `shouldBe` testMaybe output "(ABC DEF)"

      it "parses - (dash) as a delimiter" $ do
        testMaybe output "(AB-1c-5d)" `shouldBe` Just (Output "AB-1c-5d")
      
      it "parses _ (underscore) as a delimiter" $ do
        testMaybe output "(AB_1c_5d)" `shouldBe` Just (Output "AB_1c_5d")

      it "does not parse a valid input" $ do
        testMaybe output "[INPUT]" `shouldBe` Nothing

    describe "Comment parser" $ do
      it "returns the contents of the comment" $ do
        testMaybe comments "!! Hello world !!" `shouldBe` Just ([" Hello world "])

      it "skips comments and parses input" $ do
        testMaybe (comments >> input) "!! Hello world !![Hello]" `shouldBe` Just (Input "Hello")
      
      it "fails to skip invalid comments" $ do
        testMaybe (comments >> input) "## This fails ## " `shouldBe` Nothing

      it "parses multiline comments" $ do
        let expected = Just ([" This is a multiline comment ", " It always needs to be bookended by "])
            actual = testMaybe comments multilineComment
        actual `shouldBe` expected
    
    describe "Ladder parser" $ do
      it "parses a simple ladder diagram" $ do
        let expected = Just ([And (Not (Input "A")) (Output "B")])
            actual = testMaybe parseLadder "##---[/A]---(B)---##"
        actual `shouldBe` expected

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

      it "parses a ladder with a single OR" $ do
        let actual = testMaybe parseLadder simpleOrs
        actual `shouldBe` (Just [Or (Input "B") (Input "A")])

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

      it "parses a ladder with sequential OR wires" $ do
        let eb = Or (Input "E") (Input "B")
            fc = Or (Input "F") (Input "C")
            aebfc = And (And (Input "A") eb) fc
            expected = Just [And aebfc (Output "D")]
            actual = testMaybe parseLadder sequentialOrs
        actual `shouldBe` expected

      it "parses a ladder with spanning ORs" $ do
        let eb = Or (Input "E") (Input "B")
            ceb = And eb (Input "C")
            fceb = Or (Input "F") ceb
            afceb = And (Input "A") fceb
            expected = Just [And afceb (Output "D")]
            actual = testMaybe parseLadder spanningOrs
        actual `shouldBe` expected
