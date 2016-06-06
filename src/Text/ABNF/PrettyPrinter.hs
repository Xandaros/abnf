{-|
Module      : Text.ABNF.ABNF.PrettyPrinter
Description : Pretty printer for ABNF rules
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : portable
-}

module Text.ABNF.PrettyPrinter where

import Data.List (intersperse)
import qualified Data.Text as Text

import Text.ABNF.ABNF.Types

class Pretty a where
    prettyShow :: a -> String

instance Pretty a => Pretty [a] where
    prettyShow = unlines . map prettyShow

instance Pretty Rule where
    prettyShow (Rule ident definedAs sumSpec) = Text.unpack ident
                                             ++ " "
                                             ++ prettyShow definedAs
                                             ++ " "
                                             ++ prettyShow sumSpec

instance Pretty DefinedAs where
    prettyShow Equals = "="
    prettyShow Adds = "=/"

instance Pretty SumSpec where
    prettyShow (SumSpec prodspecs) = concat $
        intersperse " / " (map prettyShow prodspecs)

instance Pretty ProductSpec where
    prettyShow (ProductSpec reps) = concat $
        intersperse " " (map prettyShow reps)

instance Pretty Repetition where
    prettyShow (Repetition repeat elem) = prettyShow repeat
                                       ++ prettyShow elem

instance Pretty Repeat where
    prettyShow (Repeat 1 (Just 1)) = ""
    prettyShow (Repeat 0 Nothing) = "*"
    prettyShow (Repeat l Nothing)  = show l ++ "*"
    prettyShow (Repeat l (Just h)) = show l ++ "*" ++ show h

instance Pretty Element where
    prettyShow (RuleElement' ident) = Text.unpack ident
    prettyShow (RuleElement rule) = prettyShow rule
    prettyShow (GroupElement group) = "(" ++ prettyShow group ++ ")"
    prettyShow (OptionElement option) = "[" ++ prettyShow option ++ "]"
    prettyShow (LiteralElement lit) = prettyShow lit

instance Pretty Group where
    prettyShow (Group sumSpec) = prettyShow sumSpec

instance Pretty Literal where
    prettyShow (CharLit lit) = "\"" ++ Text.unpack lit ++ "\""
    prettyShow (NumLit lit) = prettyShow lit

instance Pretty NumLit where
    prettyShow (IntLit lit) = "%d" ++ concat (intersperse "." (map show lit))
    prettyShow (RangeLit lit1 lit2) = "%d" ++ show lit1 ++ "-" ++ show lit2

