module Main where

import            Text.LadderLogic.Parser
import            Text.LadderLogic.Repl
import            Text.LadderLogic.Types

import            Control.Monad
import            System.Environment
import            System.IO
import            Text.Trifecta

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No input file provided!"
        (f:_) -> load f

-- | Load a file and open a REPL with the program
load :: FilePath -> IO ()
load path = do
  contents <- readFile path
  case parseString parseLadder mempty contents of
    Failure err -> putStrLn $ "Error parsing file: " ++ (show err)
    Success (logic:_) -> do
      putStrLn $ "Loaded " ++ path ++ "..."
      replize repl (makeReplState contents logic) logic