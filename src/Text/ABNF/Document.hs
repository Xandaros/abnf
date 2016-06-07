module Text.ABNF.Document
    (
    -- * Document types
    -- | Re-exported from "Text.ABNF.Document.Types"
      Identifier
    , Document(..)
    , Content(..)
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
    , squashContent
    -- * Parsing documents
    -- | Re-exported from "Text.ABNF.Document.Parser"
    , generateParser
    , parseDocument
    ) where

import Text.ABNF.Document.Types ( Identifier
                                , Document(..)
                                , Content(..)
                                )

import Text.ABNF.Document.Operations ( filterDocument
                                     , squashDocument
                                     , squashDocumentOn
                                     , squashContent
                                     )

import Text.ABNF.Document.Parser ( generateParser
                                 , parseDocument
                                 )
