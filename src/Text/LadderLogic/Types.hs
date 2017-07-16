{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LadderLogic.Types where

import            Control.Monad.Except
import            Control.Monad.State
import qualified  Data.ByteString.Char8 as BS
import            Data.Function             (on)
import            Data.Functor              (fmap)
import            Data.Int
import            Data.List
import            Data.Ord
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

{- The segment interface is internal -}

-- | A segment is a ladder logic statement that has a start and end position
-- in the text
newtype Segment
  = Segment { getSegment :: (Logic, (Position, Position))}
  deriving (Show, Eq)

type Position = Int64

isNoOpSegment :: Segment -> Bool
isNoOpSegment segment =
  case intoLogic segment of
    NoOp  -> True
    _     -> False

-- | The start of the segment
segmentStart :: Segment -> Position
segmentStart = fst . snd . getSegment

-- | The end of the segment
segmentEnd :: Segment -> Position
segmentEnd = snd . snd . getSegment

-- | Get the segment position as a tuple
segmentPosition :: Segment -> (Position, Position)
segmentPosition seg = (segmentStart seg, segmentEnd seg)

-- | Returns true if the larger spans the smaller
spans :: Segment -> Segment -> Bool
spans larger smaller =
  (segmentStart smaller) >= (segmentStart larger) &&
  (segmentEnd smaller) <= (segmentEnd larger) &&
  (segmentPosition larger) /= (segmentPosition smaller)

-- | Grab the logic
intoLogic :: Segment -> Logic
intoLogic = fst . getSegment

-- | Create a segment from a Logic and a start-end Position
intoSegment :: Logic -> (Position, Position) -> Segment
intoSegment logic (s, e) = Segment (logic, (s, e))

-- | AND segments together
andSegment :: [Segment] -> Segment
andSegment segs =
  let logics = fmap intoLogic segs
  in Segment (foldAnd logics, (s, e))
  where s = segmentStart $ head $ sortBy (comparing segmentStart) $ segs
        e = segmentEnd   $ last $ sortBy (comparing segmentEnd)   $ segs

orStack :: [Segment] -> Segment
orStack segs =
  let grouped = groupSegmentsBy segmentPosition segs
      positions = (segmentPosition . head) <$> grouped
      logics = (fmap . fmap) intoLogic grouped
      ors = zipWith intoSegment (fmap foldOr logics) positions
  in andSegment ors

segmentLogic :: [Segment] -> Segment
segmentLogic segments = 
  let spanned = segments >>= (\s -> filter (spans s) segments)
  in case spanned of
    [] -> orStack segments
    _  -> let leftovers = segments \\ spanned
          in segmentLogic $ (orStack spanned) : leftovers

-- | Group segments using a function f to create a type a from the segments
groupSegmentsBy :: Ord a => (Segment -> a) -> [Segment] -> [[Segment]]
groupSegmentsBy f segments = 
  let kvs = fmap (\s -> (f s, s)) segments
      groups = Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs]
  in fmap (\kv -> snd kv) (Map.toList groups)

{- Compiler types -}

newtype Pin = Pin Int
  deriving (Eq, Ord, Show)

type Variables = Map.Map Pin Logic

data Program = Program
             { vars :: Variables
             , stack :: BS.ByteString
             }

repr :: Program -> String
repr = BS.unpack . stack

newtype CompilerError = CompilerError String

newtype CompilerT m a =
  CompilerT { runCompilerT :: ExceptT CompilerError (StateT Program m) a}
  deriving (Functor, Applicative, Monad,
            MonadState Program, MonadError CompilerError)

-- | Add to the program stack
push :: Monad m => Char -> CompilerT m ()
push w = do
  ws <- gets stack
  modify (\prog -> prog { stack = w `BS.cons` ws })