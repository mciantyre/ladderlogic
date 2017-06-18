{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LadderLogic.Compiler where

import Text.LadderLogic.Types
import Text.LadderLogic.Parser

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Functor

newtype ProgramT m a =
  ProgramT { runProgramT :: StateT [String] m a }
  deriving (Functor, Applicative, Monad)