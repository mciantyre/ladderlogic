module Text.LadderLogic.Compiler.Arduino where

import Text.LadderLogic.Types

import Control.Monad
import Control.Monad.Except

arduinoCompiler :: Monad m => Logic -> CompilerT m ()
arduinoCompiler logic = do
  case logic of
    Input i   -> push 'i' >> (forM_ i push)
    Output o  -> push 'o' >> (forM_ o push)
    
    And left right  -> arduinoCompiler left >> arduinoCompiler right >> push '&'
    Or left right   -> arduinoCompiler left >> arduinoCompiler right >> push '|'
    Not n           -> arduinoCompiler n    >> push '!'

arduinoValidation :: Monad m => Pin -> CompilerT m Pin
arduinoValidation (Pin p) =
  if (0 <= p) && (p <= 13)
  then return $ Pin p
  else throwError (CompilerError "Text.LadderLogic.Compiler.arduinoValidation received invalid pin")