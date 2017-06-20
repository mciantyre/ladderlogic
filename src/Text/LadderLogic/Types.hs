module Text.LadderLogic.Types where

import            Data.Function             (on)
import            Data.Functor              (fmap)
import            Data.Int
import            Data.List
import qualified  Data.List.NonEmpty as NEL
import            Data.Ord
import qualified  Data.Map.Strict as Map

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

-- | Fold a list of logics into one logic
foldLogic :: (Logic -> Logic -> Logic) 
          -> NEL.NonEmpty Logic 
          -> Logic
foldLogic f logics = foldl f (NEL.head logics) (NEL.tail logics)

-- | Apply AND over the collection of logics
foldAnd :: NEL.NonEmpty Logic -> Logic
foldAnd = foldLogic And

-- | Apply OR over the collection of logics
foldOr :: NEL.NonEmpty Logic -> Logic
foldOr = foldLogic Or


{- The segment interface is internal -}

-- | A segment is a ladder logic statement that has a start and end position
-- in the text
newtype Segment
  = Segment { getSegment :: (Logic, (Position, Position))}
  deriving (Show)

type Position = Int64

-- | The start of the segment
start :: Segment -> Position
start = fst . snd . getSegment

-- | The end of the segment
end :: Segment -> Position
end = snd . snd . getSegment

pos :: Segment -> (Position, Position)
pos seg = (start seg, end seg)

-- Grab the logic
intoLogic :: Segment -> Logic
intoLogic = fst . getSegment

andSegment :: NEL.NonEmpty Segment -> Segment
andSegment segs =
  let logics = fmap intoLogic segs
  in Segment (foldAnd logics, (s, e))
  where s = start $ NEL.head $ NEL.sortBy (comparing start) $ segs
        e = end   $ NEL.last $ NEL.sortBy (comparing end)   $ segs

groupSegmentsBy :: Ord a => (Segment -> a) -> [Segment] -> [[Segment]]
groupSegmentsBy f segments = 
  let kvs = fmap (\s -> (f s, s)) segments
      groups = Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs]
  in fmap (\kv -> snd kv) (Map.toList groups)