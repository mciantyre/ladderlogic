{-# LANGUAGE OverloadedStrings #-}

module Text.LadderLogic.Compiler
( module Text.LadderLogic.Compiler.Arduino
, makeCompiler
, runCompilation
, defaultValidation
, defaultCompiler
)
where

import Text.LadderLogic.Compiler.Arduino

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

emptyProgram :: Program
emptyProgram = Program Map.empty BS.empty

runCompilation :: Monad m => CompilerT m a -> m (Either CompilerError a, Program)
runCompilation compiler =
  (runStateT . runExceptT . runCompilerT) compiler emptyProgram

intoPin :: Monad m => Logic -> CompilerT m Pin
intoPin logic =
  case logic of
    Input i  -> return $ Pin (read i)
    Output o -> return $ Pin (read o)
    _        -> throwError (CompilerError "Text.LadderLogic.Compiler.intoPin cannot interpret logic as a pin")

defaultValidation :: Monad m => Pin -> CompilerT m Pin
defaultValidation = return

defaultCompiler :: Monad m => Logic -> CompilerT m ()
defaultCompiler logic = forM_ (show logic) push

addVariable :: Monad m => Pin -> Logic -> CompilerT m ()
addVariable p l = do
  vs <- gets vars
  if p `Map.member` vs
  then throwError $ CompilerError ("Text.LadderLogic.Compiler.addVariable reassignment of variable: " ++ (show l))
  else modify (\prog -> prog {vars = (Map.insert p l vs)})

validateWith :: Monad m => (Pin -> CompilerT m Pin) -> Logic -> CompilerT m ()
validateWith validation logic =
  case logic of
    Input _ -> do
      p <- intoPin logic >>= validation
      addVariable p logic

    Output _ -> do
      p <- intoPin logic >>= validation
      addVariable p logic

    And left right  -> validateWith validation left >> validateWith validation right
    Or left right   -> validateWith validation left >> validateWith validation right
    Not n           -> validateWith validation n

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