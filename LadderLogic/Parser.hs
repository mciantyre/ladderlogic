{-# LANGUAGE OverloadedStrings #-}

module LadderLogic.Parser
( parseInput
, parseOutput
, skipComments
, parseRung
, parseEmptyRung
) where

import LadderLogic.Types
import Control.Applicative
import Data.Functor
import Text.Trifecta

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
parseInput = try (brackets p)
          <|> between (char '[' *> char '/') (char ']') np
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

-- | Skip all comments
skipComments :: Parser String
skipComments = do
    commentDelimiter
    manyTill anyChar (try commentDelimiter)

-- | Rungs are delimited with || on either side
-- example: ||-----[IN]----(OUT)--||
borders :: Parser Char
borders = char '|' *> char '|'

-- | Parse the contents of a wire
parseWire :: Parser [Logic]
parseWire = some (between wires wires (parseInput <|> parseOutput))

-- | Parse an empty rung
parseEmptyRung :: Parser ()
parseEmptyRung = do
    borders
    manyTill (oneOf "| ") (try borders)
    return ()

-- | Apply the ANDing logic to sequential elements on the wire
andLogic :: [Logic] -> Parser Logic
andLogic logics =
    case logics of
        (x:[]) -> return x
        (x:xs) -> return $ foldl And x xs
        []     -> fail "andLogic received empty logics!"

-- | Parse a parallel wire
parseParallelWire :: Parser [Logic]
parseParallelWire = between pplus pplus parseWire
    where pplus = char '+'

parseParallelRung :: Parser Logic
parseParallelRung = between bss bss parseParallelWire >>= andLogic
    where bss = borders *> spaces

-- | Parse a normal rung
-- example: ||------[INPUT]--[/NOT]--(OUTPUT)-||
parseRung :: Parser Logic
parseRung = between borders borders parseWire >>= andLogic
