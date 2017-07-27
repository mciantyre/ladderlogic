{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LadderLogic.Types where

import            Control.Applicative
import            Control.Monad.Except
import            Control.Monad.IO.Class
import            Control.Monad.Reader
import            Control.Monad.State
import qualified  Data.ByteString.Char8 as BS
import            Data.Int
import qualified  Data.Map.Strict as Map

-- | Ladder logic... uh... logic!
data Logic
  = NoOp                  -- ^ no operation
  | Input String          -- ^ an input
  | Output String         -- ^ an output
  | And Logic Logic       -- ^ ANDing of two logic blocks
  | Or Logic Logic        -- ^ ORing of two logic blocks
  | Not Logic             -- ^ NOTing a logic block
  deriving (Show)

instance Eq Logic where
  NoOp == NoOp                = True
  (Input x) == (Input y)      = x == y
  (Output x) == (Output y)    = x == y
  (Not x) == (Not y)          = x == y

  -- And may match to the flip of itself, since the operation is
  -- commutative
  (And r s) == (And x y) = 
    ((r == x) && (s == y)) || ((r == y) && (s == x))

  -- Or may match to the flip of itself, since the operation is
  -- commutative
  (Or r s) == (Or x y) =
    ((r == x) && (s == y)) || ((r == y) && (s == x))

  (==) _ _ = False

-- | Remove NoOps from the logic
simplify :: Logic -> Logic
simplify logic =
  case logic of

    -- Get the explicit NoOps from binary ops
    And left NoOp   -> simplify left
    And NoOp right  -> simplify right
    Or left NoOp    -> simplify left
    Or NoOp right   -> simplify right

    -- Handle NOT
    Not NoOp        -> NoOp
    Not center      -> Not $ simplify center

    -- Simplify through the AND and ORs
    -- without infinite recursion
    And left right  -> let logic' = And (simplify left) (simplify right)
                       in if logic == logic'
                          then logic            -- at our simplest form
                          else simplify logic'  -- there's still simplification

    Or left right   -> let logic' = Or (simplify left) (simplify right)
                       in if logic == logic'
                          then logic            -- at the simplest form
                          else simplify logic'  -- there's still simplification
    -- catch-all
    _               -> logic

-- | Apply AND over the collection of logics
foldAnd :: [Logic] -> Logic
foldAnd = simplify . (foldl And NoOp)

-- | Apply OR over the collection of logics
foldOr :: [Logic] -> Logic
foldOr = simplify . (foldl Or NoOp)

-- | A mapping of strings to their Logic statement
type Variables = Map.Map String Logic

-- | The program maintains variables and a representative stack
data Program = Program
             { vars :: Variables
             , stack :: BS.ByteString
             }

-- | Get the stack
repr :: Program -> String
repr = BS.unpack . stack

-- | A compiler error is thrown if there is an issue during compilation
newtype CompilerError = CompilerError String
  deriving (Show, Eq, Monoid)

-- | The compiler runs in a stateful, error-prone context
newtype CompilerT m a =
  CompilerT { runCompilerT :: ExceptT CompilerError (StateT Program m) a}
  deriving (Functor, Applicative, Monad, Alternative,
            MonadState Program, MonadError CompilerError)

-- | Add to the program stack
push :: Monad m => Char -> CompilerT m ()
push w = do
  ws <- gets stack
  modify (\prog -> prog { stack = w `BS.cons` ws })

-- | Throw a compiler error
compilerError :: (Monad m) => String -> CompilerT m a
compilerError msg = throwError $ CompilerError msg

-- | The values maintained in the REPL
data ReplState = ReplState
               { vals :: Map.Map String Bool
               , ladder :: String
               }

-- | The REPL type is an interactive environment for manipulating a ladder
-- logic program.
newtype ReplT m a =
  ReplT { runReplT :: StateT ReplState (ReaderT Logic m) a }
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadState ReplState, MonadReader Logic)

-- | Run a REPL type with an initial REPL state. Based on the provided Logic
replize :: (Monad m) => ReplT m a -> ReplState -> Logic -> m a
replize r s l = do
  let ms = runReplT r
      mr = runStateT ms s
  (a, _) <- runReaderT mr l
  return a
  
