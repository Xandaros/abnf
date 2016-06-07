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
data Document a = Document Text.Text [Content a]
    deriving (Show, Eq)

data Content a = Terminal a
               | NonTerminal (Document a)
             deriving (Show, Eq)

instance Functor Document where
    fmap f (Document ident conts) = Document ident $ fmap (fmap f) conts

instance Functor Content where
    fmap f (Terminal a) = Terminal $ f a
    fmap f (NonTerminal doc) = NonTerminal $ fmap f doc

instance Applicative Content

instance Monad Content where
    return = Terminal
    Terminal a >>= f = f a
    NonTerminal (Document ident cs) >>= f =  NonTerminal (Document ident (map (>>= f) cs))
