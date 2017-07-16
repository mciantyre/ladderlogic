module CompilerSpec where

import            Text.LadderLogic.Compiler
import            Text.LadderLogic.Types

import            Test.Hspec

spec =
  describe "Default compiler" $ do
    it "represents the logic as a string" $ do
      let logic = And (Or (Input "13") (Input "14")) (Output "15")
          expected = show logic
          compiler = makeCompiler defaultValidation defaultCompiler
      (_, program) <- runCompilation $ compiler logic
      let actual = repr program
      actual `shouldBe` expected