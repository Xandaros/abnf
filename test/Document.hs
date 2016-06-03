{-# LANGUAGE OverloadedStrings #-}
module Document where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Test.Tasty
import Test.Tasty.HUnit

import Text.ABNF.Parser.Types
import Text.ABNF.Document
import Text.ABNF.Document.Types

import Util

testDoc :: (Eq a, Show a) => Parser a -> T.Text -> a -> Assertion
testDoc parser input expected = case parseOnly parser input of
                                  Left msg -> assertFailure msg
                                  Right a -> assertEqual "" expected a

documentTests :: TestTree
documentTests = testGroup "Document Parser" $
    [ let testLit l = testDoc (parseLiteral l)
      in  testGroup "literal" $
          [ testCase "IntLit" $
              testLit wsLiteral " " [Terminal " "]
          , testCase "IntLit concat" $
              testLit (NumLit (IntLit [0x20,0x20])) "  " [Terminal "  "]
          , testCase "RangeLit left edge" $
              testLit (NumLit (RangeLit 0x41 0x5A)) "A" [Terminal "A"]
          , testCase "RangeLit mid" $
              testLit (NumLit (RangeLit 0x41 0x5A)) "L" [Terminal "L"]
          , testCase "RangeLit right edge" $
              testLit (NumLit (RangeLit 0x41 0x5A)) "Z" [Terminal "Z"]
          , testCase "CharLit" $
              testLit (CharLit " ") " " [Terminal " "]
          ]
    , let testElem l = testDoc (parseElem l)
      in  testGroup "element" $
          [ testCase "RuleElement" $
              testElem (RuleElement $ simpleRule "x" wsElement) " "
                [NonTerminal (Document "x" [Terminal " "])]
          , testCase "GroupElement" $
              testElem (GroupElement (Group (simpleSum wsElement))) " "
                [Terminal " "]
          , testCase "OptionElement - empty" $
              testElem (OptionElement (Group (simpleSum wsElement))) ""
                []
          , testCase "OptionElement - non-empty" $
              testElem (OptionElement (Group (simpleSum wsElement))) " "
                [Terminal " "]
          , testCase "LiteralElement" $
              testElem wsElement " " [Terminal " "]
          ]
    , let testRep l = testDoc (parseRepetition l)
      in  testGroup "repetition" $
          [ testCase "single" $
              testRep (single wsElement) " " [Terminal " "]
          , testCase "1-3 - 1" $
              testRep (Repetition (Repeat 1 (Just 3)) wsElement) " "
                [Terminal " "]
          , testCase "1-3 - 2" $
              testRep (Repetition (Repeat 1 (Just 3)) wsElement) "  "
                [Terminal " ", Terminal " "]
          , testCase "1-3 - 3" $
              testRep (Repetition (Repeat 1 (Just 3)) wsElement) "   "
                [Terminal " ", Terminal " ", Terminal " "]
          , testCase "1-* - 1" $
              testRep (Repetition (Repeat 1 Nothing) wsElement) " "
                [Terminal " "]
          , testCase "1-* - 3" $
              testRep (Repetition (Repeat 1 Nothing) wsElement) "   "
                [Terminal " ", Terminal " ", Terminal " "]
          , testCase "* - 0" $
              testRep (Repetition (Repeat 0 Nothing) wsElement) ""
                []
          , testCase "* - 3" $
              testRep (Repetition (Repeat 0 Nothing) wsElement) "   "
                [Terminal " ", Terminal " ", Terminal " "]
          ]
    , let testProd l = testDoc (parseProdSpec l)
      in  testGroup "concatenation" $
          [ testCase "simple" $
              testProd (simpleProd wsElement) " "
                [Terminal " "]
          , testCase "two" $
              testProd (ProductSpec [single wsElement, single wsElement]) "  "
                [Terminal " ", Terminal " "]
          ]
    , let testSum l = testDoc (parseSumSpec l)
      in  testGroup "alternation" $
          [ testCase "simple" $
              testSum (simpleSum wsElement) " "
                [Terminal " "]
          , testCase "3-choice - 1st" $
              testSum (SumSpec [ simpleProd wsElement
                               , simpleProd (lit "a")
                               , simpleProd (lit "b")
                               ]) " "
                [Terminal " "]
          , testCase "3-choice - 2nd" $
              testSum (SumSpec [ simpleProd wsElement
                               , simpleProd (lit "a")
                               , simpleProd (lit "b")
                               ]) "a"
                [Terminal "a"]
          , testCase "3-choice - 3rd" $
              testSum (SumSpec [ simpleProd wsElement
                               , simpleProd (lit "a")
                               , simpleProd (lit "b")
                               ]) "b"
                [Terminal "b"]
          , testCase "group - 1st" $
              testSum (SumSpec [ProductSpec [ single wsElement
                                            , single $ GroupElement (Group (SumSpec
                                                [ simpleProd (lit "a")
                                                , simpleProd (lit "b")
                                                ]))
                                            ]]) " a"
                                            [Terminal " ", Terminal "a"]
          , testCase "group - 2nd" $
              testSum (SumSpec [ProductSpec [ single wsElement
                                            , single $ GroupElement (Group (SumSpec
                                                [ simpleProd (lit "a")
                                                , simpleProd (lit "b")
                                                ]))
                                            ]]) " b"
                                            [Terminal " ", Terminal "b"]
          ]
    ]

lit :: T.Text -> Element
lit = LiteralElement . CharLit

simpleRule :: T.Text -> Element -> Rule
simpleRule ident = Rule ident Equals . simpleSum

wsLiteral :: Literal
wsLiteral = NumLit $ IntLit [0x20]
