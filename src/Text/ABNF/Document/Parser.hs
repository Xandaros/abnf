{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.ABNF.Document.Parser
Description : Tools to parse ABNF into a Document
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : non-portable

This module takes a canonicalized rule to parse a document described by it.
You use the function 'generateParser' to generate an attoparsec parser.
-}

module Text.ABNF.Document.Parser where

import Control.Applicative (liftA2, (<|>), many)
import Control.Monad (join, mzero)
import Data.Char (chr)
import Data.Foldable (asum)
import Data.Monoid ((<>))

import qualified Data.Text as T
import Data.Attoparsec.Text

import Text.ABNF.ABNF.Types
import Text.ABNF.Document.Types

-- | Convenience function to directly parse a 'Document'
parseDocument :: Rule                            -- ^ 'Rule' to parse against
              -> T.Text                          -- ^ 'Text' to parse
              -> Either String (Document T.Text) -- ^ Return a 'String'
                                                 -- describing the error or the
                                                 -- successfully parsed
                                                 -- 'Document'.
                                                 --
                                                 -- The format of the 'String'
                                                 -- is as returned by
                                                 -- <https://hackage.haskell.org/package/attoparsec attoparsec>.
parseDocument = parseOnly . generateParser

-- | Generate an <https://hackage.haskell.org/package/attoparsec attoparsec> parser
generateParser :: Rule -> Parser (Document T.Text)
generateParser = parseRule

parseRule :: Rule -> Parser (Document T.Text)
parseRule (Rule ident _ spec) = Document ident <$> parseSumSpec spec <?> "Rule"

parseSumSpec :: SumSpec -> Parser [Content T.Text]
parseSumSpec (SumSpec prodspecs) = asum (map parseProdSpec prodspecs) <?> "Sum"

parseProdSpec :: ProductSpec -> Parser [Content T.Text]
parseProdSpec (ProductSpec reps) =
    join <$> (sequence $ map parseRepetition reps) <?> "Product"

parseRepetition :: Repetition -> Parser [Content T.Text]
-- any number of times
parseRepetition (Repetition (Repeat 0 Nothing) elem) =
    join <$> (many $ parseElem elem)
-- Zero times
parseRepetition (Repetition (Repeat 0 (Just 0)) _) = pure []
-- Less than n times
parseRepetition (Repetition (Repeat 0 (Just n)) elem) = do
    el <- (Just <$> parseElem elem) <|> pure Nothing
    case el of
      Just el' -> liftA2 (++) (pure el')
                    (parseRepetition (Repetition (Repeat 0 (Just (n-1))) elem))
      Nothing -> pure []
-- Between n and m times
parseRepetition (Repetition (Repeat n (Just m)) elem) =
    liftA2 (++) (parseElem elem)
                (parseRepetition (Repetition (Repeat (n-1) (Just (m-1))) elem))
-- At least n times
parseRepetition (Repetition (Repeat n x) elem) =
    liftA2 (++) (parseElem elem)
                (parseRepetition (Repetition (Repeat (n-1) x) elem))

parseElem :: Element -> Parser [Content T.Text]
parseElem (RuleElement rule) = toList . NonTerminal <$> parseRule rule <?> "Rule element"
parseElem (RuleElement' ruleName) = fail . T.unpack $ "Unknown rule: " <> ruleName
parseElem (GroupElement (Group spec)) = parseSumSpec spec <?> "Group element"
parseElem (OptionElement (Group spec)) = parseSumSpec spec <|> pure [] <?> "Optional element"
parseElem (LiteralElement lit) = parseLiteral lit <?> "Literal element"

parseLiteral :: Literal -> Parser [Content T.Text]
parseLiteral (CharLit lit) = toList . Terminal <$> asciiCI lit <?> "String literal"
parseLiteral (NumLit lit) = toList . Terminal <$> parseNumLit lit

parseNumLit :: NumLit -> Parser T.Text
parseNumLit (IntLit ints) = (T.pack <$> (sequence (char . chr <$> ints)) <?> "Int-defined character")
parseNumLit (RangeLit x1 x2) = T.pack . toList <$> (oneOf $ chr <$> [x1..x2]) <?> "Range literal"

toList :: a -> [a]
toList = pure

oneOf :: String -> Parser Char
oneOf = foldr (<|>) mzero . fmap char
