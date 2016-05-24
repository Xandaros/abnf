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

data Element = RuleElement Identifier
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
