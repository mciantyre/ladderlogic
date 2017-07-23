module Main where

import qualified  Data.ByteString.Char8 as BS
import            Control.Monad
import            System.Environment
import            System.IO
import            Text.LadderLogic
import            Text.Trifecta

main :: IO ()
main = do
  paths <- getArgs
  case paths of
    (p:ps) -> do 
      contents <- parseFromFile parseLadder p
      case contents of
        Nothing -> return ()
        Just logics -> do
          let compiler = makeCompiler defaultValidation defaultCompiler
          eps <- forM (fmap compiler logics) runCompilation
          forM_ eps showprog
          
    [] -> do
      putStrLn "No file provided!"
      return ()

  where showprog (err, program) = case err of
            Left (CompilerError msg) -> print msg
            Right _ ->  print $ repr program