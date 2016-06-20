{-|
Module      : Text.ABNF.Document
Description : Documents according to an ABNF definition
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Text.ABNF.Document
    (
    -- * Document types
    -- | Re-exported from "Text.ABNF.Document.Types"
      Document(..)
    -- * Reducing documents
    -- | Re-exported from "Text.ABNF.Document.Operations"
    --
    -- In most cases, you don't want to work with the full tree of a 'Document'.
    -- You can use these cases to 'filterDocument' away any branches you do not
    -- need and 'squashDocumentOn' those, where you don't need it as
    -- fine-grained.
    --
    -- This is incredibly useful if you have rules that parse single characters.
    , filterDocument
    , squashDocument
    , squashDocumentOn
    , lookupDocument
    , lookupDocument'
    , getContent
    -- * Parsing documents
    -- | Re-exported from "Text.ABNF.Document.Parser"
    , generateParser
    , parseDocument
    ) where

import Text.ABNF.Document.Types ( Document(..)
                                )

import Text.ABNF.Document.Operations ( filterDocument
                                     , squashDocument
                                     , squashDocumentOn
                                     , getContent
                                     , lookupDocument
                                     , lookupDocument'
                                     )

import Text.ABNF.Document.Parser ( generateParser
                                 , parseDocument
                                 )
