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

-- | A 'Document' represents a tree of the parsed input. Directly after parsing,
-- every 'Rule' that was applied, gets a node in the tree.
--
-- You can use 'Text.ABNF.Document.filterDocument' and
-- 'Text.ABNF.Document.squashDocumentOn' to reduce the tree into a more
-- managable shape.
data Document a = Document Text.Text [Document a]
                | Terminal a
                deriving (Show, Eq)

instance Functor Document where
    fmap f (Document ident conts) = Document ident $ fmap (fmap f) conts
    fmap f (Terminal a) = Terminal $ f a
