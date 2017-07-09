module Text.LadderLogic.Parser where

import            Text.LadderLogic.Types

import            Control.Applicative
import qualified  Data.List.NonEmpty as NEL
import            Data.Maybe (catMaybes)
import            Text.Trifecta
import            Text.Trifecta.Delta

{- Simple parsers -}

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

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

{- Comment parsing -}

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

{- Logic parsing -}

-- | Define a wire
wire :: Parser Char
wire = char '-'

wires = many wire

parseTag :: Parser (Maybe Logic)
parseTag = some wire *> optional (input <|> output)

-- | Rungs are delimited with ## on either side
-- example: ##-----[IN]----(OUT)--##
borders :: Parser Char
borders = char '#' *> char '#'

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
  segs <- some $ optional $ makeSegment $ between wires wires inout <* (many (char '+') <|> some borders)
  skipEOL
  return $ catMaybes segs
  where inout = input <|> output

-- | A dangling segment hangs off of the main wire to create OR behavior
parseDanglingSegments :: Parser [Segment]
parseDanglingSegments = do
  borders
  spacebars
  segs <- many (makeSegment dangling <* spacebars)
  borders
  return segs
  where spacebars = skipMany (oneOf " |")
        dangling = between (plus *> wires) (wires <* plus) (input <|> output)
        plus = char '+'

-- | Parse one rung. A rung consists of the main wire with zero or more
-- dangling segments
parseRung :: Parser [Segment]
parseRung = do
  segs <- parseMainSegment
  dangle <- many (try parseDanglingSegments <* skipEOL)
  return $ segs ++ (concat dangle)

-- | Parse the whole diagram
parseLadder :: Parser [Logic]
parseLadder = do
  skipEOL
  skipMany comments
  logics <- some $ fmap NEL.fromList parseRung >>= (return . orSegment)
  skipEOL
  return $ intoLogic <$> logics