{-|
Module      : Text.ABNF.ABNF.Parser
Description : ABNF Parser
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : portable

This module provides facilities to parse ABNF documents.
To parse documents using ABNF, see "Text.ABNF.Document.Parser"

The parser you will most likely be interested in is 'rulelist'
-}
module Text.ABNF.ABNF.Parser where

import Prelude hiding (repeat)

import Data.Char (ord, isHexDigit, digitToInt)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Numeric (readInt)
import Text.Megaparsec
import Text.Megaparsec.Text

import Text.ABNF.ABNF.Types

identifier :: Parser Identifier
identifier = Text.pack <$> do
    firstChar <- letterChar
    otherChars <- many $ alphaNumChar <|> char '-'
    pure (firstChar:otherChars)

-- | Convencience function to directly parse a list of rules.
-- This is equivalent to
--
-- @
-- 'parse' 'rulelist'
-- @
parseABNF :: String -> Text.Text -> Either (ParseError Char Dec) [Rule]
parseABNF = parse rulelist

-- | The main parser, which parses a list of 'Rule's.
rulelist :: Parser [Rule]
rulelist = catMaybes <$> (some $ Just <$> rule <|> (many wsp *> c_nl *> pure Nothing))

rule :: Parser Rule
rule = Rule <$> identifier
            <*> defined_as
            <*> elements
            <* c_nl

defined_as :: Parser DefinedAs
defined_as = many c_wsp *> ((try (string "=/") *> pure Adds)
                         <|> try (string "=")  *> pure Equals) <* many c_wsp

elements :: Parser SumSpec
elements = alternation <* many wsp

c_wsp :: Parser String
c_wsp = sequence [wsp] <|> (try $ do
    newl <- c_nl
    white <- wsp
    pure $ newl ++ [white])

c_nl :: Parser String
c_nl = comment <|> crlf

comment :: Parser String
comment = char ';' *> many (wsp <|> vchar) <* crlf

alternation :: Parser SumSpec
alternation = (do
    first <- concatenation
    rest <- many (try (many c_wsp *> char '/') *> many c_wsp *> concatenation)
    pure . SumSpec $ first:rest) <?> "alternation"

concatenation :: Parser ProductSpec
concatenation = (do
    first <- repetition
    rest <- many (try $ some c_wsp *> repetition)
    pure . ProductSpec $ first:rest) <?> "concatenation"

repetition :: Parser Repetition
repetition = Repetition <$> repeat <*> element

repeat :: Parser Repeat
repeat = try asteriskNumbers <|> try singleNumber <|> pure (Repeat 1 (Just 1))
    where
        singleNumber = Repeat 1 <$> (Just . read <$> some digitChar)
        asteriskNumbers = do
            firstNumber <- option 0 (read <$> some digitChar)
            char '*'
            secondNumber <- optional (read <$> some digitChar)
            pure $ Repeat firstNumber secondNumber

element :: Parser Element
element = RuleElement' <$> identifier
      <|> GroupElement <$> group
      <|> OptionElement <$> option_
      <|> LiteralElement <$> literal

group :: Parser Group
group = Group <$>
    (char '(' *> many c_wsp *> alternation <* many c_wsp <* char ')')

option_ :: Parser Group
option_ = Group <$>
    (char '[' *> many c_wsp *> alternation <* many c_wsp <* char ']')

literal :: Parser Literal
literal = CharLit <$> char_val <|> NumLit <$> num_val <|> CharLit <$> prose_val

char_val :: Parser Text.Text
char_val = Text.pack <$> (char '"' *> many schar <* char '"')
    where
        schar = satisfy (\c -> ord c >= 0x20 && ord c <= 0x21
                            || ord c >= 0x23 && ord c <= 0x7E)

num_val :: Parser NumLit
num_val = char '%' *> (bin_val <|> dec_val <|> hex_val)

{-# WARNING bin_val "readBinInt is unsafe" #-}
bin_val :: Parser NumLit
bin_val = num_val' 'b' binInt
    where
        readBinInt :: String -> Int
        readBinInt = fst . head . readInt 2
            (`elem` ['0', '1'])
            digitToInt
        binInt = readBinInt <$> many (char '0' <|> char '1')

dec_val :: Parser NumLit
dec_val = num_val' 'd' readInt
    where
        readInt :: Parser Int
        readInt = read <$> some digitChar

{-# WARNING hex_val "readHexInt is unsafe" #-}
hex_val :: Parser NumLit
hex_val = num_val' 'x' hexInt
    where
        readHexInt :: String -> Int
        readHexInt = fst . head . readInt 16 isHexDigit digitToInt
        hexInt = readHexInt <$> many hexDigitChar

num_val' :: Char -> Parser Int -> Parser NumLit
num_val' c hexInt = do
    char c
    digits <- hexInt
    intLit digits <|> rangeLit digits <|> pure (IntLit [digits])
    where
        intLit' :: Parser [Int]
        intLit' = some $ char '.' *> hexInt

        intLit first = do
            rest <- intLit'
            pure $ IntLit (first:rest)

        rangeLit :: Int -> Parser NumLit
        rangeLit startRange = do
            char '-'
            endRange <- hexInt
            pure $ RangeLit startRange endRange

        --readHexInt :: String -> Int
        --readHexInt = fst . head . readInt 16 isHexDigit digitToInt
        --hexInt = readHexInt <$> many hexDigitChar

prose_val :: Parser Text.Text
prose_val = Text.pack <$> (char '<' *> many pchar <* char '>')
    where
        pchar = satisfy (\c -> ord c >= 0x20 && ord c <= 0x3D
                            || ord c >= 0x3F && ord c <= 0x7E)

vchar :: Parser Char
vchar = satisfy (\c -> ord c >= 0x21 && ord c <= 0x7E)

wsp :: Parser Char
wsp = char ' ' <|> char '\t'
