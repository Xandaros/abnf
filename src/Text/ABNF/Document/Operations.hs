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

import Control.Monad (join)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text

import Text.ABNF.Document.Types

-- | Filter documents according to some predicate.
-- Similar to 'filter' in the Prelude.
filterDocument :: forall a. (Document a -> Bool) -- ^ Predicate to check
               -> Document a                     -- ^ Document to filter
               -> Maybe (Document a)             -- ^ Returns 'Nothing' if the
                                                 --   predicate fails, cascades
                                                 --   otherwise
filterDocument predicate term@(Terminal _) 
  | predicate term = Just term
  | otherwise = Nothing

filterDocument predicate doc@(Document ident conts)
    | predicate doc = Just . Document ident $ (catMaybes . fmap (filterDocument predicate) $ conts)
    | otherwise = Nothing

-- | Squash all contents of a 'Document' into a single 'Terminal'
squashDocument :: Monoid a => Document a -> Document a
squashDocument term@(Terminal _) = term
squashDocument doc@(Document ident _) = Document ident [Terminal $ getContent doc]

getContent :: Monoid a => Document a -> a
getContent (Terminal a) = a
getContent (Document _ conts) = mconcat (fmap getContent conts)

-- | Squash all contents of a 'Document' which matches the predicate
-- See also 'squashDocument'
squashDocumentOn :: forall a. Monoid a => (Document a -> Bool) -> Document a -> Document a
squashDocumentOn predicate doc@(Document ident conts)
    | predicate doc = squashDocument doc
    | otherwise = Document ident (squashDocumentOn predicate <$> conts)
squashDocumentOn _ term@(Terminal _) = term

-- | Looks up nested 'Document's with a particular identifier.
-- NB: Will not recurse into matching documents.
lookupDocument :: forall a. Text.Text -- ^ Identifier to search for
               -> Document a          -- ^ 'Document' to search in
               -> [Document a]
lookupDocument _ (Terminal _) = []
lookupDocument ident doc@(Document ident2 conts)
    | ident2 == ident = [doc]
    | otherwise       = join $ lookupDocument ident <$> conts

-- | Similar to 'lookupDocument', 'lookupDocument'' will find a 'Document' with
-- a particular identifier. This, however, will only find immediate children and
-- will not recurse.
lookupDocument' :: forall a. Text.Text -- ^ Identifier to search for
                -> Document a          -- ^ 'Document' to search in
                -> [Document a]
lookupDocument' _ (Terminal _) = []
lookupDocument' ident' (Document _ conts) = catMaybes $ go ident' <$> conts
    where
        go _     (Terminal _) = Nothing
        go ident doc@(Document ident2 _) | ident == ident2 = Just doc
                                         | otherwise = Nothing
