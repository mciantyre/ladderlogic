{-# LANGUAGE RecordWildCards #-}

module Text.LadderLogic.Repl where

import            Text.LadderLogic.Parser
import            Text.LadderLogic.Types

import            Control.Monad.IO.Class
import            Control.Monad.Reader
import            Control.Monad.State
import            Data.List                 (intercalate)
import qualified  Data.Map.Strict as Map
import            System.IO
import            Text.Trifecta

type Repl = ReplT IO

printString :: String -> IO ()
printString str = putStr str >> hFlush stdout

prompt :: String -> IO String
prompt p = printString p >> getLine

quits :: [String]
quits = ["quit", ":q", "exit"]

emptyReplState :: ReplState
emptyReplState = ReplState Map.empty []

type Action = [String] -> Repl ()

actions :: [(String, Action)]
actions = [ ("open"   , setInput True)
          , ("close"  , setInput False)
          , ("show"   , showValues)
          , ("ladder" , showLadder)
          , ("logic"  , showLogic)
          , ("help"   , showHelp)
          ]

actionWords :: String
actionWords =
  let acts = map fst actions
  in intercalate ", " acts

showActions :: IO ()
showActions = putStrLn $ "Possible actions are " ++ actionWords

showHelp :: Action
showHelp _ = do
  liftIO $ putStrLn "A LadderLogic REPL"
  liftIO showActions
  liftIO $ putStrLn $ "To exit, type one of " ++ intercalate ", " quits

setInput :: Bool -> Action
setInput _ [] = return ()
setInput b (s:ss) = do
  vs <- gets vals
  if Map.member s vs
  then modify (\rs -> rs { vals = Map.insert s b vs } ) >> setInput b ss
  else liftIO $ putStrLn $ "Not a variable: " ++ s

showValues :: Action
showValues _ = do
  vs <- gets vals
  liftIO $ forM_ (map show $ Map.assocs vs) putStrLn

showLadder :: Action
showLadder _ = do
  l <- gets ladder
  liftIO $ putStrLn l

showLogic :: Action
showLogic _ = do
  l <- ask
  liftIO $ putStrLn (show l)

handleInput :: String -> Repl ()
handleInput input = do
  if null input
  then return ()
  else
    let (w:ws) = words input
    in case lookup w actions of
      Just action -> action ws
      Nothing     -> liftIO $ putStrLn ("Unknown command: " ++ w) >> showActions

repl :: Repl ()
repl = do
  input <- liftIO $ prompt "Ladder>> "
  if input `elem` quits
  then liftIO $ putStrLn "Goodbye!\n"
  else handleInput input >> repl

makeReplState :: String -> Logic -> ReplState
makeReplState s l = ReplState (loop l Map.empty) s
  where loop :: Logic -> Map.Map String Bool -> Map.Map String Bool
        loop l m =
          case l of
            NoOp        -> m
            Input i     -> Map.insert i False m
            Output o    -> Map.insert o False m
            Not n       -> loop n m
            And l r     -> loop l (loop r m)
            Or l r      -> loop l (loop r m)

load :: FilePath -> IO ()
load path = do
  contents <- readFile path
  case parseString parseLadder mempty contents of
    Failure err -> putStrLn $ "Error parsing file: " ++ (show err)
    Success (logic:_) -> replize repl (makeReplState contents logic) logic