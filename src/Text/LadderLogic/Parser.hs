module Text.LadderLogic.Parser where

import            Text.LadderLogic.Types

import            Control.Applicative
import qualified  Data.List.NonEmpty as NEL
import            Text.Trifecta
import            Text.Trifecta.Delta

-- | The only special characters to be used in tag names
special :: Parser Char
special = oneOf "_-+"

-- | The parser for a tag allows for letters, digits, and the special characters
tag :: Parser [Char]
tag = some (alphaNum <|> special)

-- | Map the Logic contructor into the parser
asLogic :: (String -> Logic) -> Parser Logic
asLogic = flip fmap tag

-- | Parse an input tag
input :: Parser Logic
input = try (brackets p) <|> between (char '[' *> char '/') (char ']') np
  where p  = asLogic Input
        np = asLogic (Not . Input)

-- | Parse an output tag
output :: Parser Logic
output = parens (asLogic Output)

-- | Skip the wire symbols connectors
wires :: Parser ()
wires = skipMany (oneOf "-")

-- | Comments are delimited with !! on either end
-- example: !! This is a comment !!
commentDelimiter :: Parser Char
commentDelimiter = char '!' *> char '!'

-- | Parse comments in between !!s
comments :: Parser [String]
comments = 
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
insandouts :: Parser Logic
insandouts =
  fmap NEL.fromList (some (between wires wires (input <|> output)))
    >>= (return . foldAnd)

-- | Acquire the start and end position of a ladder logic parser,
-- and create a segment. We can upgrade a wire parser into a segment parser
-- with this method.
makeSegment :: Parser Logic -> Parser Segment
makeSegment parser = do
  p0    <- position
  logic <- parser
  p1    <- position
  return $ Segment (logic, (column p0, column p1))

parseMainSegment :: Parser [Segment]
parseMainSegment = do
  borders
  many $ (makeSegment insandouts) <* (char '+' <|> borders)

-- | A dangling segment hangs off of the main wire to create OR behavior
parseDanglingSegments :: Parser [Segment]
parseDanglingSegments = do
  borders
  sb
  segs <- many (dangling <* sb)
  borders
  return segs
  where sb = skipMany (oneOf " |")
        dangling = between plus plus (makeSegment insandouts)
        plus = char '+'

-- | Parse one rung. A rung consists of the main wire with zero or more
-- dangling segments
parseRung :: Parser [Segment]
parseRung = do
  segs <- parseMainSegment
  skipEOL
  dangle <- many (try parseDanglingSegments <* skipEOL)
  return $ segs ++ (concat dangle)

-- | Apply the ORing logic to parallel elements of the rung
orLogic :: [Segment] -> Parser Logic
orLogic segments = 
  let grouped = groupSegmentsBy pos segments
      ors = fmap oring grouped
  in (return . foldAnd) (NEL.fromList ors)
  where oring segs = case segs of
              (x:[]) -> intoLogic x
              (x:xs) -> foldl Or (intoLogic x) (fmap intoLogic xs) 

-- | Parse the whole diagram
parseLadder :: Parser [Logic]
parseLadder = do
  skipEOL
  skipMany comments
  logics <- some (parseRung >>= orLogic)
  skipEOL
  return logics
