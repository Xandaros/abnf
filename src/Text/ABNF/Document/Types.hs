{-|
Module      : Text.ABNF.Document.Types
Description : Document type
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : portable
-}
module Text.ABNF.Document.Types where

import qualified Data.Text as Text

type Identifier = Text.Text

data Document = Document Identifier [Content]
    deriving (Show, Eq)

data Content = Terminal Text.Text
             | NonTerminal Document
             deriving (Show, Eq)
