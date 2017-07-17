module Text.LadderLogic.Compiler.Arduino where

import Text.LadderLogic.Types

import Control.Monad
import Control.Monad.Except
import Data.Char

arduinoCompiler :: Monad m => Logic -> CompilerT m ()
arduinoCompiler logic = do
  case logic of
    Input i   -> push 'i' >> (forM_ i push)
    Output o  -> push 'o' >> (forM_ o push)
    
    And left right  -> arduinoCompiler left >> arduinoCompiler right >> push '&'
    Or left right   -> arduinoCompiler left >> arduinoCompiler right >> push '|'
    Not n           -> arduinoCompiler n    >> push '!'

arduinoValidation :: Monad m => Logic -> CompilerT m Logic
arduinoValidation = checkDigits >=> checkBounds

checkDigits :: (Monad m) => Logic -> CompilerT m (Int, Logic)
checkDigits logic =
  case logic of
    Input i  -> check i
    Output o -> check o
    _        -> compilerError $ "arduinoValidation invalid invocation with logic " ++ (show logic)
  where check :: (Monad m) => String -> CompilerT m (Int, Logic)
        check s = if and $ map isDigit s
                  then return $ (read s, logic)
                  else compilerError $ "arduinoValidation cannot discern pin: " ++ (show logic)

checkBounds :: (Monad m) => (Int, Logic) -> CompilerT m Logic
checkBounds (p, logic) =
  if (0 <= p) && (p <= 13)
  then return logic
  else compilerError $ "arduinoValidation logic out of Arduino pin range: " ++ (show logic)