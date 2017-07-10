{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import            Text.LadderLogic.Parser
import            Text.LadderLogic.Types

import            Test.Hspec
import            Text.RawString.QQ
import qualified  Text.Trifecta as T

parseTestString :: T.Parser a -> String -> T.Result a
parseTestString p s = T.parseString p mempty s

failedParse :: T.Result a -> Bool
failedParse (T.Success _) = False
failedParse _           = True

instance (Eq a) => Eq (T.Result a) where
  (T.Success x) == (T.Success y) = x == y
  (==) _ _ = False

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
        parseTestString input "[INPUT]" `shouldBe` (T.Success (Input "INPUT"))
      
      it "parses the NOT indicator ('/') from brackets" $ do
        let expected = T.Success (Not (Input "INPUT"))
            actual = parseTestString input "[/INPUT]"
        actual `shouldBe` expected

      it "does not parse empty identifiers" $ do
        parseTestString input "[/]" `shouldSatisfy` failedParse

      it "parses numbers in identifiers" $ do
        parseTestString input "[1234567]" `shouldBe` T.Success (Input "1234567")
      
      it "does not parse letters and numbers in identifiers" $ do
        parseTestString input "[ABC123]" `shouldBe` T.Success (Input "ABC123")

      it "does not parse spaces in identifiers" $ do
        parseTestString input "[ABC DEF]" `shouldSatisfy` failedParse

      it "parses - (dash) as a delimiter" $ do
        T.Success (Not (Input "AB-1c-5d")) `shouldBe` parseTestString input "[/AB-1c-5d]"

      it "parses _ (underscore) as a delimiter" $ do
        parseTestString input "[/AB_1c_5d]" `shouldBe` T.Success (Not (Input "AB_1c_5d"))

      it "does not parse a valid output" $ do
        parseTestString input "(OUTPUT)" `shouldSatisfy` failedParse

    describe "Output parser" $ do
      it "parses the output name from parentheses" $ do
        let expected = T.Success (Output "OUTPUT")
            actual = parseTestString output "(OUTPUT)"
        actual `shouldBe` expected

      it "does not parse empty identifiers" $ do
        parseTestString output "()" `shouldSatisfy` failedParse

      it "parses numbers in identifiers" $ do
        parseTestString output "(1234567)" `shouldBe` T.Success (Output "1234567")

      it "does not parse letters and numbers in identifiers" $ do
        parseTestString output "(ABC123)" `shouldBe` T.Success (Output "ABC123")

      it "does not parse spaces in identifiers" $ do
        parseTestString output "(ABC DEF)" `shouldSatisfy` failedParse

      it "parses - (dash) as a delimiter" $ do
        parseTestString output "(AB-1c-5d)" `shouldBe` T.Success (Output "AB-1c-5d")
      
      it "parses _ (underscore) as a delimiter" $ do
        parseTestString output "(AB_1c_5d)" `shouldBe` T.Success (Output "AB_1c_5d")

      it "does not parse a valid input" $ do
        parseTestString output "[INPUT]" `shouldSatisfy` failedParse

    describe "Comment parser" $ do
      it "returns the contents of the comment" $ do
        parseTestString comments "!! Hello world !!" `shouldBe` T.Success ([" Hello world "])

      it "skips comments and parses input" $ do
        parseTestString (comments >> input) "!! Hello world !![Hello]" `shouldBe` T.Success (Input "Hello")
      
      it "fails to skip invalid comments" $ do
        parseTestString (comments >> input) "## This fails ## " `shouldSatisfy` failedParse

      it "parses multiline comments" $ do
        let expected = T.Success ([" This is a multiline comment ", " It always needs to be bookended by "])
            actual = parseTestString comments multilineComment
        actual `shouldBe` expected
    
    describe "Ladder parser" $ do
      it "parses a simple ladder diagram" $ do
        let expected = T.Success ([And (Not (Input "A")) (Output "B")])
            actual = parseTestString parseLadder "##---[/A]---(B)---##"
        actual `shouldBe` expected

      it "parses a ladder diagram with unrelated rungs" $ do
        let expected = T.Success ([And (Not (Input "A")) (Output "B"),
                              And (Input "C") (Output "D")])
            actual = parseTestString parseLadder unrelatedRungs
        actual `shouldBe` expected

      it "parses a ladder with ORing wires" $ do
        let bc = And (Input "B") (Input "C")
            ebc = Or (Input "E") bc
            aebc = And (Input "A") ebc
            expected = T.Success [And aebc (Output "D")]
            actual = parseTestString parseLadder diagramWithOring
        actual `shouldBe` expected

      it "parses a ladder with a single OR" $ do
        let actual = parseTestString parseLadder simpleOrs
        actual `shouldBe` (T.Success [Or (Input "B") (Input "A")])

      it "parses a multi-rung ladder program" $ do
        let bc = Or (Not (Input "C")) (Input "B")
            abc = And (Not (Input "A")) bc
            rung1 = And (abc) (Output "D")
            rung2 = And (Not (Input "F")) (Output "G")
            expected = T.Success [rung1, rung2]
            actual = parseTestString parseLadder multipleRungs
        actual `shouldBe` expected

      it "parses a ladder with multiple OR wires" $ do
        let ec = Or (Input "E") (Input "C")
            bce = Or ec (Input "B")
            abce = And (Input "A") bce
            expected = T.Success [And abce (Output "D")]
            actual = parseTestString parseLadder multipleOrs
        actual `shouldBe` expected

      it "parses a ladder with sequential OR wires" $ do
        let eb = Or (Input "E") (Input "B")
            fc = Or (Input "F") (Input "C")
            aebfc = And (And (Input "A") eb) fc
            expected = T.Success [And aebfc (Output "D")]
            actual = parseTestString parseLadder sequentialOrs
        actual `shouldBe` expected

      it "parses a ladder with spanning ORs" $ do
        let eb = Or (Input "E") (Input "B")
            ceb = And eb (Input "C")
            fceb = Or (Input "F") ceb
            afceb = And (Input "A") fceb
            expected = T.Success [And afceb (Output "D")]
            actual = parseTestString parseLadder spanningOrs
        actual `shouldBe` expected
