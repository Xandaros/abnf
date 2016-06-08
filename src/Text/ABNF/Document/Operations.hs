{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Text.ABNF.Document.Operations
Description : Some operations on documents
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : ScopedTypeVariables
-}

module Text.ABNF.Document.Operations where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as Text

import Text.ABNF.Document.Types

-- | Filter documents according to some predicate.
-- Similar to 'filter' in the Prelude.
filterDocument :: forall a. (Document a -> Bool) -- ^ Predicate to check
               -> Document a                     -- ^ Document to filter
               -> Maybe (Document a)             -- ^ Returns 'Nothing' if the
                                                 --   predicate fails, cascades
                                                 --   otherwise
filterDocument pred doc@(Document ident conts) | pred doc = Just . Document ident $ (catMaybes . fmap filterNT $ conts)
                                               | otherwise = Nothing
    where
        filterNT :: Content a -> Maybe (Content a)
        filterNT a@(Terminal _) = Just a
        filterNT (NonTerminal doc) | pred doc = NonTerminal <$> filterDocument pred doc
                                   | otherwise = Nothing

-- | Squash all contents of a 'Document' into a single 'Terminal'
squashDocument :: Monoid a => Document a -> Document a
squashDocument (Document ident conts) = Document ident [Terminal $ squashContent conts]

-- | Squash all contents of a 'Document' which matches the predicate
-- See also 'squashDocument'
squashDocumentOn :: forall a. Monoid a => (Document a -> Bool) -> Document a -> Document a
squashDocumentOn pred doc@(Document ident conts) | pred doc = squashDocument doc
                                                 | otherwise = Document ident (squashNT <$> conts)
    where
        squashNT :: Content a -> Content a
        squashNT (Terminal a) = Terminal a
        squashNT (NonTerminal doc) = NonTerminal $ squashDocumentOn pred doc

-- | Squash all contents using the 'Monoid' instance of @a@, cascading into
-- 'NonTerminal's.
squashContent :: Monoid a => [Content a] -> a
squashContent [] = mempty
squashContent ((Terminal a):xs) = a <> squashContent xs
squashContent ((NonTerminal (Document _ conts)):xs) = squashContent conts <> squashContent xs

lookupDocument :: forall a. Text.Text -> Document a -> Maybe (Document a)
lookupDocument ident doc@(Document ident2 conts) | ident2 == ident = Just doc
                                                 | otherwise = lookupNT conts
    where
        lookupNT :: [Content a] -> Maybe (Document a)
        lookupNT [] = Nothing
        lookupNT ((Terminal _):xs) = lookupNT xs
        lookupNT ((NonTerminal d):xs) = lookupDocument ident d <|> lookupNT xs

