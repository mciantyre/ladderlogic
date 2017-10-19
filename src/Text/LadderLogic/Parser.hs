module Text.LadderLogic.Parser where

import            Text.LadderLogic.Types
import            Text.LadderLogic.Types.Internal

import            Control.Applicative
import            Data.Maybe (catMaybes)
import            Text.Trifecta
import            Text.Trifecta.Delta

{- Simple parsers -}

-- | The only special characters to be used in tag names
special :: Parser Char
special = oneOf "_-+"

-- | The parser for a tag allows for letters, digits, and the special characters
allowable :: Parser [Char]
allowable = some (alphaNum <|> special)

-- | Map the Logic contructor into the parser
asLogic :: (String -> Logic) -> Parser Logic
asLogic = flip fmap allowable

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

fork :: Parser Char
fork = char '+'

-- | Rungs are delimited with ## on either side
-- example: ##-----[IN]----(OUT)--##
borders :: Parser Char
borders = char '#' *> char '#'

tags :: Parser Logic
tags = some iotags >>= (return . foldAnd . catMaybes)
  where iotags = some wire *> optional (input <|> output)

-- | Acquire the start and end position of a ladder logic parser,
-- and create a segment. We can upgrade a wire parser into a segment parser
-- with this method.
segment :: Parser Logic -> Parser Segment
segment parser = do
  p0    <- position
  logic <- parser
  p1    <- position
  return $ Segment (logic, (column p0, column p1))

parseMainSegments :: Parser [Segment]
parseMainSegments = do
  borders
  seg <- segment tags
  segs <- many $ (fork *> segment tags)
  borders
  skipEOL
  return (seg : segs)

-- | A dangling segment hangs off of the main wire to create OR behavior
parseDanglingSegments :: Parser [Segment]
parseDanglingSegments = do
  borders
  spacebars
  segs <- many $ (fork *> segment tags) <* fork <* spacebars
  borders
  skipEOL
  return segs
  where spacebars = skipMany (oneOf " |")

-- | Parse one rung. A rung consists of the main wire with zero or more
-- dangling segments
parseRung :: Parser [Segment]
parseRung = do
  segs <- parseMainSegments
  dangle <- many (try parseDanglingSegments)
  return $ filter (not . isNoOpSegment) (segs ++ (concat dangle))

-- | Parse the whole diagram
parseLadder :: Parser [Logic]
parseLadder = do
  skipEOL
  skipMany comments
  logics <- some $ parseRung >>= (return . segmentCollapse)
  skipEOL
  return $ intoLogic <$> logics