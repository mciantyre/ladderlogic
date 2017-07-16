module Text.LadderLogic.Types.Internal where

import Text.LadderLogic.Types

import            Data.Function             (on)
import            Data.Functor              (fmap)
import            Data.Int                  (Int64)
import            Data.List                 ((\\), sortBy)
import            Data.Ord                  (comparing)
import qualified  Data.Map.Strict as Map

-- | A segment is a ladder logic statement that has a start and end position
-- in the text
newtype Segment
  = Segment { getSegment :: (Logic, (Position, Position))}
  deriving (Show, Eq)

type Position = Int64

-- | check if the segment is a NoOp
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

-- | Handle a collection of segments with stacked elements, and apply ORing
-- to the stacked elements
orStack :: [Segment] -> Segment
orStack segs =
  let grouped = groupSegmentsBy segmentPosition segs
      positions = (segmentPosition . head) <$> grouped
      logics = (fmap . fmap) intoLogic grouped
      ors = zipWith intoSegment (fmap foldOr logics) positions
  in andSegment ors

-- | Handle segment collapsing into one larger segment
segmentCollapse :: [Segment] -> Segment
segmentCollapse segments = 
  let spanned = segments >>= (\s -> filter (spans s) segments)
  in case spanned of
    [] -> orStack segments
    _  -> let leftovers = segments \\ spanned
          in segmentCollapse $ (orStack spanned) : leftovers

-- | Group segments using a function f to create a type a from the segments
groupSegmentsBy :: Ord a => (Segment -> a) -> [Segment] -> [[Segment]]
groupSegmentsBy f segments = 
  let kvs = fmap (\s -> (f s, s)) segments
      groups = Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs]
  in fmap (\kv -> snd kv) (Map.toList groups)