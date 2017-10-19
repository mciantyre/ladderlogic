{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.LadderLogic.Repl
( setInput
, makeReplState
, makeReplEnv
, repl
, updateOutputs
, logicToLogicTypes
, logicToInitialValues
) where

import            Text.LadderLogic.Parser
import            Text.LadderLogic.Types

import            Control.Monad.IO.Class
import            Control.Monad.Reader
import            Control.Monad.State
import            Data.List                 (intercalate)
import qualified  Data.Map.Strict as Map
import            System.IO
import            Text.Trifecta

-- | Print a string and flush the output
printString :: MonadIO m => String -> m ()
printString str = liftIO (putStr str) >> liftIO (hFlush stdout)

-- | Display the prompt and receive input
prompt :: MonadIO m => String -> m String
prompt p = printString p >> liftIO getLine

-- | Statements used for quitting the REPL
quits :: [String]
quits = ["quit", ":q", "exit"]

-- | An Action accepts a list of arguments and executes in the REPL
type Action m = [String] -> ReplT m ()

-- | A mapping of action words to the actual actions
actions :: MonadIO m => [(String, Action m)]
actions = [ ("true"   , setInput True)
          , ("false"  , setInput False)
          , ("values" , const showValues)
          , ("ladder" , const showLadder)
          , ("logic"  , const showLogic)
          , ("help"   , const showHelp)
          ]
 
-- | Display possible actions
showActions :: forall m. MonadIO m => m ()
showActions = liftIO $ putStrLn ("Possible actions are " ++ actionWords)
  where actionWords :: String
        actionWords = let acts = fmap fst (actions :: [(String, Action m)])
                      in intercalate ", " acts

-- | Display a help message
showHelp :: MonadIO m => ReplT m ()
showHelp = do
  liftIO $ putStrLn "A LadderLogic REPL"
  liftIO showActions
  liftIO $ putStrLn $ "To exit, type one of " ++ intercalate ", " quits
  
-- | Set the input(s) to the provided value
setInput :: MonadIO m => Bool -> Action m
setInput _ [] = return ()
setInput b (s:ss) = do
  vs <- gets vals
  ts <- asks (logicToLogicTypes . replLogic)
  case Map.lookup s ts of
    Just (Input _)  ->
      modify (\rs -> rs { vals = Map.insert s b vs } ) >> setInput b ss
    Just (Output _) -> skipWithMsg $ "Cannot mutate output " ++ s
    _               -> skipWithMsg $ "Not a variable: " ++ s
  where
    skipWithMsg msg = do
      liftIO $ putStrLn msg
      setInput b ss

-- | Show the current values of the REPL
showValues :: MonadIO m => ReplT m ()
showValues = do
  updateValues
  vs <- gets vals
  ts <- asks (logicToLogicTypes . replLogic)
  let tvs = Map.intersectionWith (,) ts vs  
  liftIO $ forM_ (map show (Map.elems tvs)) putStrLn

-- | Display the ladder from which the REPL derives
showLadder :: MonadIO m => ReplT m ()
showLadder = do
  l <- asks replLadder
  liftIO $ putStrLn l

-- | Display the logic of the program
showLogic :: MonadIO m => ReplT m ()
showLogic = do
  l <- asks replLogic
  liftIO $ putStrLn (show l)

-- | Lookup an action based on the input
handleInput :: MonadIO m => String -> ReplT m ()
handleInput input = do
  if null input
  then return ()
  else
    let (w:ws) = words input
    in case lookup w actions of
      Just action -> action ws
      Nothing     -> liftIO $ putStrLn ("Unknown command: " ++ w) >> showActions

-- | The REPL loops until quit
repl :: MonadIO m => ReplT m ()
repl = do
  showHelp
  loop
  where loop = do
          input <- liftIO $ prompt "Ladder>> "
          if input `elem` quits
          then liftIO $ putStrLn "Goodbye!\n"
          else handleInput input >> loop

-- | Create an initial REPL state
makeReplState :: Logic -> ReplState
makeReplState lgic = ReplState (logicToInitialValues lgic)

-- | Make a ReplEnv
makeReplEnv :: String -> Logic -> ReplEnv
makeReplEnv = ReplEnv

-- | Convert a logic into a map of Input / Output designations
logicToMapUsing :: (Logic -> a)       -- ^ A transforming function
                -> Map.Map String a   -- ^ An initial map
                -> Logic              -- ^ The logic to put into the map
                -> Map.Map String a   
logicToMapUsing f m lgic =
  case lgic of
    NoOp        -> m
    Input i     -> Map.insert i (f lgic) m
    Output o    -> Map.insert o (f lgic) m
    Not n       -> logicToMapUsing f m n
    And l r     -> logicToMapUsing f (logicToMapUsing f m r) l
    Or l r      -> logicToMapUsing f (logicToMapUsing f m r) l

-- | Create a map with all values initially set to False
logicToInitialValues :: Logic -> Map.Map String Bool
logicToInitialValues = logicToMapUsing (const False) Map.empty

-- | Create a map with the keys mapping to the Input / Output Logic type
logicToLogicTypes :: Logic -> Map.Map String Logic
logicToLogicTypes = logicToMapUsing id Map.empty

-- | Update the output tags based on the input tags
updateValues :: MonadIO m => ReplT m ()
updateValues = do
  vs <- gets vals
  lgic <- asks replLogic
  let vs' = updateOutputs lgic vs
  modify (\rs -> rs { vals = vs' } )

-- | Pure implementation of updateValues
updateOutputs :: Logic -> Map.Map String Bool -> Map.Map String Bool
updateOutputs lgic vs =
  let ts = logicToLogicTypes lgic
      outs = Map.filter outputs ts
      outvs = Map.intersection vs outs
      outvs' = Map.map (const $ evaluate lgic vs) outvs
  in Map.union outvs' vs
  where outputs l = case l of Output _ -> True
                              _        -> False

-- | Evaluate a ladder logic program with a map of inputs to current values
evaluate :: Logic -> Map.Map String Bool -> Bool
evaluate (And l (Output _)) m = evaluate l m
evaluate (And (Output _) r) m = evaluate r m
evaluate lgic m =
  case lgic of
    And l r   -> (evaluate l m) && (evaluate r m)
    Or l r    -> (evaluate l m) || (evaluate r m)
    Not n     -> not (evaluate n m)
    Input i   -> maybe False id (Map.lookup i m)
    Output o  -> maybe False id (Map.lookup o m)