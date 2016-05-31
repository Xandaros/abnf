{-|
Module      : Text.ABNF.Parser.Types
Description : Types used by the parser
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : portable

These types are used by the parser and are loosely modeled after the ABNF
privded in <https://tools.ietf.org/html/rfc5234#section-4 RFC 5234>
-}
module Text.ABNF.Parser.Types where

import qualified Data.Text as Text

type Identifier = Text.Text

data Rule = Rule Identifier DefinedAs SumSpec
    deriving (Show)

data SumSpec = SumSpec [ProductSpec]
    deriving (Show)

data ProductSpec = ProductSpec [Repetition]
    deriving (Show)

data Repetition = Repetition Repeat Element
    deriving (Show)

data Repeat = Repeat Int (Maybe Int)
    deriving (Show)

data Element = RuleElement' Identifier
             | RuleElement Rule
             | GroupElement Group
             | OptionElement Group
             | LiteralElement Literal
    deriving (Show)

data Group = Group SumSpec
    deriving (Show)

data Literal = CharLit Text.Text | NumLit NumLit
    deriving (Show)

data NumLit = IntLit [Int]
            | RangeLit Int Int
    deriving (Show)

data DefinedAs = Equals | Adds
    deriving (Show)
