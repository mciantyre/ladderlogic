module Text.LadderLogic.Parser where

import            Text.LadderLogic.Types

import            Control.Applicative
import            Data.Function             (on)
import            Data.Functor              (fmap)
import            Data.Int
import            Data.List
import qualified  Data.Map.Strict as Map
import            Text.Trifecta
import            Text.Trifecta.Delta

-- | The only special characters to be used in tag names
special :: Parser Char
special = oneOf "_-+"

-- | The parser for a tag allows for letters, digits, and the special characters
parseTag :: Parser [Char]
parseTag = some (alphaNum <|> special)

-- | Map the Logic contructor into the parser
parseAs :: (String -> Logic) -> Parser Logic
parseAs = flip fmap parseTag

-- | Parse an input tag
parseInput :: Parser Logic
parseInput = try (brackets p) <|> between (char '[' *> char '/') (char ']') np
  where p  = parseAs Input
        np = parseAs (Not . Input)

-- | Parse an output tag
parseOutput :: Parser Logic
parseOutput = parens (parseAs Output)

-- | Skip the wire symbols connectors
wires :: Parser ()
wires = skipMany (oneOf "-")

-- | Comments are delimited with !! on either end
-- example: !! This is a comment !!
commentDelimiter :: Parser Char
commentDelimiter = char '!' *> char '!'

-- | Parse comments in between !!s
parseComments :: Parser [String]
parseComments = 
  some $ do
    skipEOL
    commentDelimiter
    comment <- manyTill anyChar (try commentDelimiter)
    skipEOL
    return comment

-- | Rungs are delimited with ## on either side
-- example: ##-----[IN]----(OUT)--##
borders :: Parser Char
borders = char '#' *> char '#'

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- | Parse the contents of a wire
-- example : --[B]--(C)---
parseSeriesWire :: Parser Logic
parseSeriesWire =
  some (between wires wires (parseInput <|> parseOutput)) >>= andLogic

-- | A parallel wire is a normal wire bookended with +s
-- example : +--[B]--[C]--+
parseParallelWire :: Parser Logic
parseParallelWire = between plus plus parseSeriesWire
  where plus = char '+'

-- | Apply the ANDing logic to sequential elements on the wire
andLogic :: [Logic] -> Parser Logic
andLogic logics =
  case logics of
    (x:[]) -> return x
    (x:xs) -> return $ foldl And x xs
    []     -> fail "LadderLogic.Parser.andLogic received empty input list"

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

-- | Acquire the start and end position of a ladder logic parser,
-- and create a segment. We can upgrade a wire parser into a segment parser
-- with this method.
makeSegment :: Parser Logic -> Parser Segment
makeSegment parser = do
  d0    <- position
  logic <- parser
  d1    <- position
  return $ Segment (logic, (column d0, column d1))

parseSeriesSegment :: Parser Segment
parseSeriesSegment = makeSegment parseSeriesWire

parseParallelSegment :: Parser Segment
parseParallelSegment = makeSegment parseParallelWire

parseSegment :: Parser Segment
parseSegment = try parseParallelSegment <|> parseSeriesSegment

-- | A dangling segment hangs off of the main wire to create OR behavior
parseDanglingSegments :: Parser [Segment]
parseDanglingSegments = do
  borders
  sb
  segs <- many (parseParallelSegment <* sb)
  borders
  return segs
  where sb = skipMany (oneOf " |")

-- | Parse one rung. A rung consists of the main wire with zero or more
-- dangling segments
parseRung :: Parser [Segment]
parseRung = do
  seg <- between borders borders (some parseSegment)
  skipEOL
  dangle <- many (try parseDanglingSegments <* skipEOL)
  return $ seg ++ (concat dangle)

groupSegmentsBy :: Ord a => (Segment -> a) -> [Segment] -> [[Segment]]
groupSegmentsBy f segments = 
  let kvs = map (\s -> (f s, s)) segments
      groups = Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs]
  in map (\kv -> snd kv) (Map.toList groups)

-- | Apply the ORing logic to parallel elements of the rung
orLogic :: [Segment] -> Parser Logic
orLogic segments = 
  let grouped = groupSegmentsBy pos segments
      ors = map oring grouped
  in andLogic ors
  where oring segs = case segs of
              (x:[]) -> intoLogic x
              (x:xs) -> foldl Or (intoLogic x) (map intoLogic xs)

-- | Parse the whole diagram
parseLadder :: Parser [Logic]
parseLadder = do
  skipEOL
  skipMany parseComments
  logics <- some (parseRung >>= orLogic)
  skipEOL
  return logics

