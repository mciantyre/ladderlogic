{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LadderLogic.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Functor

-- | Ladder logic... uh... logic!
data Logic
  = Input String          -- ^ an input
  | Output String         -- ^ an output
  | And Logic Logic       -- ^ ANDing of two logic blocks
  | Or Logic Logic        -- ^ ORing of two logic blocks
  | Not Logic             -- ^ NOTing a logic block
  deriving (Show)

instance Eq Logic where
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

-- | A program is a collection of rungs run under some stateful monad
newtype ProgramT m a
  = ProgramT { runProgramT :: StateT [Logic] m a }
  deriving (Functor, Applicative, Monad)

-- | Our program is a monad transformer
instance MonadTrans ProgramT where
  lift = ProgramT . lift

-- | Our concrete program performs IO
-- during testing, it **wouldn't** perform IO, so we'd replace with Identity
type Program = ProgramT IO

-- | Run the ladder logic program
runLadderProgram :: Program a -> [Logic] -> IO (a, [Logic])
runLadderProgram = runStateT . runProgramT
