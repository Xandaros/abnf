{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.ABNF.Document
Description : Tools to parse ABNF into a Document
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Text.ABNF.Document (generateParser) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Char (chr)
import Data.Foldable (asum)
import Data.Monoid ((<>))

import qualified Data.Text as Text
import Text.Megaparsec
--import Text.Megaparsec.Char
import Text.Megaparsec.Text

import Text.ABNF.Parser.Types
import Text.ABNF.Document.Types

import Debug.Trace

generateParser :: Rule -> Parser Document
generateParser = parseRule

parseRule :: Rule -> Parser Document
parseRule (Rule ident _ spec) = Document ident <$> parseSumSpec spec <?> "Rule"

parseSumSpec :: SumSpec -> Parser [Content]
parseSumSpec (SumSpec prodspecs) = asum (map parseProdSpec prodspecs) <?> "Sum"

parseProdSpec :: ProductSpec -> Parser [Content]
parseProdSpec (ProductSpec reps) =
    join <$> (sequence $ map parseRepetition reps) <?> "Product"

parseRepetition :: Repetition -> Parser [Content]
-- any number of times
parseRepetition (Repetition (Repeat 0 Nothing) elem) =
    join <$> (many $ parseElem elem)
-- Zero times
parseRepetition (Repetition (Repeat 0 (Just 0)) _) = pure []
-- Less than n times
parseRepetition (Repetition (Repeat 0 (Just n)) elem) = do
    el <- try (Just <$> parseElem elem) <|> pure Nothing
    case el of
      Just el' -> liftA2 (++) (pure el')
                    (parseRepetition (Repetition (Repeat 0 (Just (n-1))) elem))
      Nothing -> pure []
-- At least n times
parseRepetition (Repetition (Repeat n x) elem) =
    liftA2 (++) (parseElem elem)
                (parseRepetition (Repetition (Repeat (n-1) x) elem))

parseElem :: Element -> Parser [Content]
parseElem (RuleElement rule) = toList . NonTerminal <$> parseRule rule <?> "Rule element"
parseElem (RuleElement' ruleName) = fail . Text.unpack $ "Unknown rule: " <> ruleName
parseElem (GroupElement (Group spec)) = parseSumSpec spec <?> "Group element"
parseElem (OptionElement (Group spec)) = parseSumSpec spec <|> pure [] <?> "Optional element"
parseElem (LiteralElement lit) = parseLiteral lit <?> "Literal element"

parseLiteral :: Literal -> Parser [Content]
parseLiteral (CharLit lit) = toList . Terminal . Text.pack <$> try (string' (Text.unpack lit)) <?> "String literal"
parseLiteral (NumLit lit) = toList . Terminal <$> parseNumLit lit

parseNumLit :: NumLit -> Parser Text.Text
parseNumLit (IntLit ints) = Text.pack <$> try (sequence (char . chr <$> ints)) <?> "Int-defined character"
parseNumLit (RangeLit x1 x2) = Text.pack . toList <$> try (oneOf $ chr <$> [x1..x2]) <?> "Range literal"

toList :: a -> [a]
toList = pure
