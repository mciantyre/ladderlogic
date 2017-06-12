{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LadderLogic.Types
( Logic(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Functor

-- | Ladder logic... uh... logic!
data Logic
    = Value String          -- ^ generic value
    | Input String          -- ^ an input
    | Output String         -- ^ an output
    | And Logic Logic       -- ^ ANDing of two logic blocks
    | Or Logic Logic        -- ^ ORing of two logic blocks
    | Not Logic             -- ^ NOTing a logic block
    deriving (Show, Eq)

-- | A rung is a collection of logical statements
data Rung = Rung
          { number    :: Int      -- ^ A rung identifier
          , logic     :: Logic    -- ^ The logic of the rung
          }

-- | A program is a collection of rungs run under some stateful monad
newtype ProgramT m a
    = ProgramT { runProgramT :: StateT [Rung] m a }
    deriving (Functor, Applicative, Monad)

-- | Our program is a monad transformer
instance MonadTrans ProgramT where
    lift = ProgramT . lift

-- | Our concrete program performs IO
-- during testing, it **wouldn't** perform IO, so we'd replace with Identity
type Program = ProgramT IO

-- | Run the ladder logic program
runLadderProgram :: Program a -> [Rung] -> IO (a, [Rung])
runLadderProgram = runStateT . runProgramT
