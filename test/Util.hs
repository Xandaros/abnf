module Util where

import Text.ABNF.Parser.Types

simpleSum :: Element -> SumSpec
simpleSum = SumSpec . (:[]) . simpleProd

simpleProd :: Element -> ProductSpec
simpleProd = ProductSpec . (:[]) . single

single :: Element -> Repetition
single = Repetition (Repeat 1 (Just 1))

wsElement :: Element
wsElement = LiteralElement (NumLit (IntLit [0x20]))
