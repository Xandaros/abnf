{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-|
Module      : Main
Description : Testsuite
Copyright   : (c) Martin Zeller, 2016
License     : BSD2
Maintainer  : Martin Zeller <mz.bremerhaven@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Main where

import Test.Tasty

import ABNF (abnfTests)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" $ [abnfTests]
