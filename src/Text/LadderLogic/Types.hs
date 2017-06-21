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
foldLogic f logics =
  foldl f (NEL.head logics) (NEL.tail logics)

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
segmentStart :: Segment -> Position
segmentStart = fst . snd . getSegment

-- | The end of the segment
segmentEnd :: Segment -> Position
segmentEnd = snd . snd . getSegment

-- | Get the segment position as a tuple
segmentPosition :: Segment -> (Position, Position)
segmentPosition seg = (segmentStart seg, segmentEnd seg)

-- | Returns true if the first segment is underneath the second segment
underneath :: Segment -> Segment -> Bool
underneath smaller bigger =
  (segmentStart smaller) >= (segmentStart bigger) &&
  (segmentEnd smaller) <= (segmentEnd bigger)

-- | Grab the logic
intoLogic :: Segment -> Logic
intoLogic = fst . getSegment

-- | Create a segment from a Logic and a start-end Position
intoSegment :: Logic -> (Position, Position) -> Segment
intoSegment logic (s, e) = Segment (logic, (s, e))

-- | AND segments together
andSegment :: NEL.NonEmpty Segment -> Segment
andSegment segs =
  let logics = fmap intoLogic segs
  in Segment (foldAnd logics, (s, e))
  where s = segmentStart $ NEL.head $ NEL.sortBy (comparing segmentStart) $ segs
        e = segmentEnd   $ NEL.last $ NEL.sortBy (comparing segmentEnd)   $ segs

intoNonEmptyLists :: [[a]] -> NEL.NonEmpty (NEL.NonEmpty a)
intoNonEmptyLists = (fmap NEL.fromList) . NEL.fromList

-- | OR segments together
orSegment :: NEL.NonEmpty Segment -> Segment
orSegment segs =
  let grouped = intoNonEmptyLists $ groupSegmentsBy segmentPosition (NEL.toList segs)
      positions = (segmentPosition . NEL.head) <$> grouped
      logics = (fmap . fmap) intoLogic grouped
      ors = NEL.zipWith intoSegment (fmap foldOr logics) positions
  in andSegment ors

-- | Group segments using a function f to create a type a from the segments
groupSegmentsBy :: Ord a => (Segment -> a) -> [Segment] -> [[Segment]]
groupSegmentsBy f segments = 
  let kvs = fmap (\s -> (f s, s)) segments
      groups = Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs]
  in fmap (\kv -> snd kv) (Map.toList groups)