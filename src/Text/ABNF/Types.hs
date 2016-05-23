module Text.ABNF.Types where

import qualified Data.Text as Text

type Identifier = Text.Text

data SyntaxTree = SyntaxTree Identifier Content

data Content = Terminal Text.Text
             | NonTerminal [SyntaxTree]
