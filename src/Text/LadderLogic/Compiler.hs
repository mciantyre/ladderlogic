{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LadderLogic.Compiler where

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

newtype Pin = Pin Int
  deriving (Eq, Ord, Show)

type Variables = Map.Map Pin Logic

data Program = Program
             { vars :: Variables
             , stack :: BS.ByteString
             }

repr :: Program -> String
repr = BS.unpack . stack

emptyProgram :: Program
emptyProgram = Program Map.empty BS.empty

data CompilerError = CompilerError String

newtype CompilerT m a =
  CompilerT { runCompilerT :: ExceptT CompilerError (StateT Program m) a}
  deriving (Functor, Applicative, Monad,
            MonadState Program, MonadError CompilerError)

runCompilation :: Monad m => CompilerT m a -> m (Either CompilerError a, Program)
runCompilation compiler =
  (runStateT . runExceptT . runCompilerT) compiler emptyProgram

-- | Add to the program stack
push :: Monad m => Char -> CompilerT m ()
push w = do
  ws <- gets stack
  modify (\prog -> prog { stack = w `BS.cons` ws })

intoPin :: Monad m => Logic -> CompilerT m Pin
intoPin logic =
  case logic of
    Input i  -> return $ Pin (read i)
    Output o -> return $ Pin (read o)
    _        -> throwError (CompilerError "Text.LadderLogic.Compiler.intoPin cannot interpret logic as a pin")

arduinoValidation :: Monad m => Pin -> CompilerT m Pin
arduinoValidation (Pin p) =
  if (0 <= p) && (p <= 13)
  then return $ Pin p
  else throwError (CompilerError "Text.LadderLogic.Compiler.arduinoValidation received invalid pin")

defaultValidation :: Monad m => Pin -> CompilerT m Pin
defaultValidation = return

addVariable :: Monad m => Pin -> Logic -> CompilerT m ()
addVariable p l = do
  vs <- gets vars
  if p `Map.member` vs
  then throwError $ CompilerError ("Text.LadderLogic.Compiler.addVariable reassignment of variable: " ++ (show l))
  else modify (\prog -> prog {vars = (Map.insert p l vs)})

validateWith :: Monad m => (Pin -> CompilerT m Pin) -> Logic -> CompilerT m ()
validateWith v logic =
  case logic of
    Input _ -> do

      p <- intoPin logic >>= v
      addVariable p logic

    Output _ -> do
      p <- intoPin logic >>= v
      addVariable p logic

    And left right  -> validateWith v left >> validateWith v right
    Or left right   -> validateWith v left >> validateWith v right
    Not n           -> validateWith v n
    
arduinoCompiler :: Monad m => Logic -> CompilerT m ()
arduinoCompiler logic = do
  case logic of
    Input i   -> push 'i' >> (forM_ i push)
    Output o  -> push 'o' >> (forM_ o push)
    
    And left right  -> arduinoCompiler left >> arduinoCompiler right >> push '&'
    Or left right   -> arduinoCompiler left >> arduinoCompiler right >> push '|'
    Not n           -> arduinoCompiler n    >> push '!'

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