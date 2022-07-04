{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
module ABNF where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec hiding (single)
import Text.Megaparsec.Char
import qualified Data.Text as T

import Text.ABNF.ABNF.Parser
import Text.ABNF.ABNF.Types
import Util

testParser :: (Eq a, Show a) => Parser a -> T.Text -> a -> Assertion
testParser parser input expected = case parse parser "" input of
                                     Left msg -> assertFailure (show msg)
                                     Right a -> assertEqual "" expected a

abnfTests :: TestTree
abnfTests = testGroup "ABNF Parser"
    [ let testLit = testParser literal
      in  testGroup "Literals" $
          [ testCase "CharLit" $ testLit "\"a\""
               (CharLit "a")
          , testCase "Long CharLit" $ testLit "\"asdfghjkl\""
               (CharLit "asdfghjkl")
          , testCase "IntLit" $ testLit "%x20"
               (NumLit $ IntLit [0x20])
          , testCase "IntLit.conat" $ testLit "%x20.20"
               (NumLit $ IntLit [0x20,0x20])
          , testCase "IntLit decimal" $ testLit "%d32"
               (NumLit $ IntLit [32])
          , testCase "IntLit binary" $ testLit "%b00100000"
               (NumLit $ IntLit [0b00100000])
          , testCase "RangeLit" $ testLit "%x41-5A"
               (NumLit $ RangeLit 0x41 0x5A)
          ]
    , let testRep = testParser repetition
      in  testGroup "Repetition" $
          [ testCase "single" $ testRep "%x20"
                (single wsElement)
          , testCase "1-2 times" $ testRep "1*2%x20"
                (Repetition (Repeat 1 (Just 2)) wsElement)
          , testCase "1-* times" $ testRep "1*%x20"
                (Repetition (Repeat 1 Nothing) wsElement)
          , testCase "7 times" $ testRep "7%x20"
                (Repetition (Repeat 7 (Just 7)) wsElement)
          , testCase "* times" $ testRep "*%x20"
                (Repetition (Repeat 0 Nothing) wsElement)
          ]
    , let testProd = testParser concatenation
      in  testGroup "Concatenation" $
          [ testCase "simple" $ testProd "%x20 %x20"
                (ProductSpec [single wsElement, single wsElement])
          , testCase "group" $ testProd "%x20 (%x20 %x20)"
                (ProductSpec [ single wsElement
                             , single $ GroupElement (Group (SumSpec
                                [ ProductSpec [ single wsElement
                                              , single wsElement
                                              ]
                                ]))
                             ])
          ]
    , let testSum = testParser elements
      in  testGroup "Elements" $
          [ testCase "simple" $ testSum "%x20"
                (SumSpec [simpleProd wsElement])
          , testCase "sum" $ testSum "%x20 / %x09"
                (SumSpec [ simpleProd wsElement
                         , simpleProd (LiteralElement . NumLit $ IntLit [0x09])])
          , testCase "complex" $ testSum "%x20 / %x20 (%x20 / %x20)"
                (SumSpec [ simpleProd wsElement
                         , ProductSpec [ single wsElement
                                       , single $ GroupElement (Group (SumSpec [simpleProd wsElement, simpleProd wsElement]))
                                       ]
                         ])
          ]
    , let testRule = testParser rule
      in  testGroup "Rule" $
          [ testCase "equals" $ testRule "a = %x20\r\n"
                (Rule "a" Equals (simpleSum wsElement))
          , testCase "adds" $ testRule "a =/ %x20\r\n"
                (Rule "a" Adds (simpleSum wsElement))
          ]
    ]
