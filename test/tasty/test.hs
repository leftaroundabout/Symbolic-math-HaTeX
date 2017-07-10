-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Math.LaTeX.Internal.MathExpr
import Text.LaTeX (LaTeX, raw, Text)
import qualified Text.LaTeX as LaTeX

import Test.Tasty
import Test.Tasty.HUnit

import CAS.Dumb

import Data.Monoid


main = defaultMain tests

type Expr = Expression LaTeX

infix 4 `shouldYieldLaTeX`
shouldYieldLaTeX :: Expr -> Text -> Assertion
shouldYieldLaTeX e s = LaTeX.render (toMathLaTeX e) @?= s


tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Rendering of simple expressions"
     [ testCase "𝑎 + 𝑏 * 𝑐" $  𝑎 + 𝑏 * 𝑐
             `shouldYieldLaTeX` "a+b{\\cdot}c"
     , testCase "(𝑎 + 𝑏) * 𝑐" $  (𝑎 + 𝑏) * 𝑐
          `shouldYieldLaTeX` "\\left(a+b\\right){\\cdot}c"
     ]
  ]



