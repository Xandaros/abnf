module Text.ABNF.Document.Types where

import qualified Data.Text as Text

type Identifier = Text.Text

data Document = Document Identifier Content

data Content = Terminal Text.Text
             | NonTerminal [Document]
