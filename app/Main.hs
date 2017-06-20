module Main where

import Control.Monad
import System.Environment
import System.IO
import Text.LadderLogic.Parser
import Text.Trifecta

main :: IO ()
main = do
  paths <- getArgs
  case paths of
    (p:ps) -> do 
      result <- parseFromFile parseLadder p
      case result of
        Nothing -> return ()
        Just a -> print $ show a
    [] -> do
      putStrLn "No file provided!"
      return ()