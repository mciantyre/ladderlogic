{-# LANGUAGE OverloadedStrings #-}

module Text.LadderLogic.Compiler
( makeCompiler
, runCompilation
, defaultValidation
, defaultCompiler
)
where

import Text.LadderLogic.Types
import Text.LadderLogic.Parser

import            Control.Applicative
import            Control.Monad
import            Control.Monad.Except
import            Control.Monad.State
import qualified  Data.ByteString.Char8 as BS
import            Data.Functor
import qualified  Data.Map.Strict as Map
import            Data.Word

-- | An empty program
emptyProgram :: Program
emptyProgram = Program Map.empty BS.empty

-- | Perform compilation with the provided compiler
runCompilation :: Monad m => CompilerT m a -> m (Either CompilerError a, Program)
runCompilation compiler =
  (runStateT . runExceptT . runCompilerT) compiler emptyProgram

-- | A default validation scheme
defaultValidation :: Monad m => Pin -> CompilerT m Pin
defaultValidation = return

-- | A default compilation mode
defaultCompiler :: Monad m => Logic -> CompilerT m ()
defaultCompiler logic = forM_ (show logic) push

-- | Add the variable into the compiler
addVariable :: Monad m => Pin -> Logic -> CompilerT m ()
addVariable p l = do
  vs <- gets vars
  let ml = Map.lookup p vs
  case ml of
    Nothing -> modify (\prog -> prog {vars = (Map.insert p l vs)})
    Just l' -> do
      guard (l' /= l)
      throwError $ CompilerError $ "Error: reassignment of logic " ++ (show l') ++ " -> " ++ (show l)
 
-- | Perform program validation with the validation function on the logic
validateWith :: Monad m => (Pin -> CompilerT m Pin) -> Logic -> CompilerT m ()
validateWith validation logic =
  case logic of
    Input i -> do
      p <- validation $ Pin (read i)
      addVariable p logic

    Output o -> do
      p <- validation $ Pin (read o)
      addVariable p logic

    And left right  -> validateWith validation left >> validateWith validation right
    Or left right   -> validateWith validation left >> validateWith validation right
    Not n           -> validateWith validation n

-- | Create a compiler type with a compiler routine and a validation routine
makeCompiler :: Monad m
             => (Pin -> CompilerT m Pin)
             -> (Logic -> CompilerT m ())
             -> Logic
             -> CompilerT m ()
makeCompiler verifier compiler =
  (\logic -> do
    validateWith verifier logic
    compiler logic
    s <- gets stack
    modify (\prog -> prog { stack = BS.reverse s }))