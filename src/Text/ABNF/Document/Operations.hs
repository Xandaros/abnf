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

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import Text.ABNF.Document.Types

filterDocument :: forall a. (Document a -> Bool) -> Document a -> Maybe (Document a)
filterDocument pred doc@(Document ident conts) | pred doc = Just . Document ident $ (catMaybes . fmap filterNT $ conts)
                                               | otherwise = Nothing
    where
        filterNT :: Content a -> Maybe (Content a)
        filterNT a@(Terminal _) = Just a
        filterNT (NonTerminal doc) | pred doc = NonTerminal <$> filterDocument pred doc
                                   | otherwise = Nothing

squashDocument :: Monoid a => Document a -> Document a
squashDocument (Document ident conts) = Document ident [Terminal $ squashContent conts]

squashDocumentOn :: forall a. Monoid a => (Document a -> Bool) -> Document a -> Document a
squashDocumentOn pred doc@(Document ident conts) | pred doc = squashDocument doc
                                                 | otherwise = Document ident (squashNT <$> conts)
    where
        squashNT :: Content a -> Content a
        squashNT (Terminal a) = Terminal a
        squashNT (NonTerminal doc) = NonTerminal $ squashDocumentOn pred doc

squashContent :: Monoid a => [Content a] -> a
squashContent [] = mempty
squashContent ((Terminal a):xs) = a <> squashContent xs
squashContent ((NonTerminal (Document _ conts)):xs) = squashContent conts <> squashContent xs
