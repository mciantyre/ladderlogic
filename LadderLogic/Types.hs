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
    = Input String          -- ^ an input
    | Output String         -- ^ an output
    | And Logic Logic       -- ^ ANDing of two logic blocks
    | Or Logic Logic        -- ^ ORing of two logic blocks
    | Not Logic             -- ^ NOTing a logic block
    deriving (Show, Eq)

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
