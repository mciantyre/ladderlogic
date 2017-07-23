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

-- | The REPL executes in an IO context
type Repl = ReplT IO

-- | Print a string and flush the output
printString :: String -> IO ()
printString str = putStr str >> hFlush stdout

-- | Display the prompt and receive input
prompt :: String -> IO String
prompt p = printString p >> getLine

-- | Statements used for quitting the REPL
quits :: [String]
quits = ["quit", ":q", "exit"]

-- | An Action accepts a list of arguments and executes in the REPL
type Action = [String] -> Repl ()

-- | A mapping of action words to the actual actions
actions :: [(String, Action)]
actions = [ ("true"   , setInput True)
          , ("false"  , setInput False)
          , ("values" , const showValues)
          , ("ladder" , const showLadder)
          , ("logic"  , const showLogic)
          , ("help"   , const showHelp)
          ]
 
-- | Display possible actions
showActions :: IO ()
showActions = putStrLn $ "Possible actions are " ++ actionWords
  where actionWords =  let acts = map fst actions
                       in intercalate ", " acts


-- | Display a help message
showHelp :: Repl ()
showHelp = do
  liftIO $ putStrLn "A LadderLogic REPL"
  liftIO showActions
  liftIO $ putStrLn $ "To exit, type one of " ++ intercalate ", " quits
  
-- | Set the input(s) to the provided value
setInput :: Bool -> Action
setInput _ [] = return ()
setInput b (s:ss) = do
  ts <- gets types
  vs <- gets vals
  case Map.lookup s ts of
    Just (Input _)  ->
      modify (\rs -> rs { vals = Map.insert s b vs } ) >> setInput b ss
    Just (Output _) -> liftIO $ putStrLn $ "Cannot mutate output " ++ s
    _               -> liftIO $ putStrLn $ "Not a variable: " ++ s

-- | Show the current values of the REPL
showValues :: Repl ()
showValues = do
  updateValues
  vs <- gets vals
  ts <- gets types
  let tvs = Map.intersectionWith (,) ts vs  
  liftIO $ forM_ (map show (Map.elems tvs)) putStrLn

-- | Display the ladder from which the REPL derives
showLadder :: Repl ()
showLadder = do
  l <- gets ladder
  liftIO $ putStrLn l

-- | Display the logic of the program
showLogic :: Repl ()
showLogic = do
  l <- ask
  liftIO $ putStrLn (show l)

-- | Lookup an action based on the input
handleInput :: String -> Repl ()
handleInput input = do
  if null input
  then return ()
  else
    let (w:ws) = words input
    in case lookup w actions of
      Just action -> action ws
      Nothing     -> liftIO $ putStrLn ("Unknown command: " ++ w) >> showActions

-- | The REPL loops until quit
repl :: Repl ()
repl = do
  input <- liftIO $ prompt "Ladder>> "
  if input `elem` quits
  then liftIO $ putStrLn "Goodbye!\n"
  else handleInput input >> repl

-- | Create an initial REPL state
makeReplState :: String -> Logic -> ReplState
makeReplState s l =
  ReplState (recurse l (const False) Map.empty) (recurse l id Map.empty) s
  where recurse :: Logic -> (Logic -> a) -> Map.Map String a -> Map.Map String a
        recurse l f m = case l of
                        NoOp        -> m
                        Input i     -> Map.insert i (f l) m
                        Output o    -> Map.insert o (f l) m
                        Not n       -> recurse n f m
                        And l r     -> recurse l f (recurse r f m)
                        Or l r      -> recurse l f (recurse r f m)

-- | Update the output tags based on the input tags
updateValues :: Repl ()
updateValues = do
  vs <- gets vals
  ts <- gets types
  l <- ask
  let outs = Map.filter outputs ts
      outvs = Map.intersection vs outs
      outvs' = Map.map (const $ evaluate l vs) outvs
      vs' = Map.union outvs' vs
  modify (\rs -> rs { vals = vs' } )
  where outputs l = case l of Output _ -> True
                              _        -> False

-- | Evaluate a ladder logic program with a map of inputs to current values
evaluate :: Logic -> Map.Map String Bool -> Bool
evaluate (And l (Output _)) m = evaluate l m
evaluate (And (Output _) r) m = evaluate r m
evaluate log m =
  case log of
    And l r   -> (evaluate l m) && (evaluate r m)
    Or l r    -> (evaluate l m) || (evaluate r m)
    Not n     -> not (evaluate n m)
    Input i   -> maybe False id (Map.lookup i m)
    Output o  -> maybe False id (Map.lookup o m)