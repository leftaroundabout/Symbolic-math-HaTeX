-- |
-- Module      : Math.LaTeX.Prelude
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>7 extensions
-- 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Math.LaTeX.Prelude (
   -- * Use in documents
     toMathLaTeX, (>$), dmaths
   -- * Primitive symbols
   , module CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps
   -- ** Modifiers
   , (%$>), prime, LaTeX.bar, LaTeX.hat, LaTeX.vec, LaTeX.underline
   -- * Operators
   , (°), (⁀), (...)
#if __GLASGOW_HASKELL__ > 802
   , (،..،), (،), (⸪=), (=⸪)
#endif
   , (␣), (+..+), (*..*), (×), (⊗), (∘), factorial
   , (◝), (◝⁀), (◞), (◞◝), (|◞), (|◝), (|◞◝)
   , (⩵), (≡), (⩵!), (≠), (⪡), (⪢), (≤), (≥), (≪), (≫), (₌₌)
   , (=→), (≈), (∼)
   , (⊂), (/⊂), (⊆), (⊃), (⊇), (∋), (∌), (∈), (∉), (∩), (∪), (-\-), (⸪), (⊕)
   , (∀:), (∃:)
   , (-→), (↦), (↪), (==>), (<==), (<=>), (∧), (∨)
   , (∫), (◞∫), (◞∮), d, (∑), (◞∑), (∏), (◞∏), del, nabla
   , (<.<), (≤.<), (<.≤), (≤.≤), (±), (∓), set, tup, intv
   , infty
   , nobreaks, matrix, cases
   -- * Algebraic manipulation
   , (&~~!)
   ) where

import CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps hiding ((%$>))
import CAS.Dumb.Symbols
import Math.LaTeX.Internal.MathExpr
import Math.LaTeX.Internal.Display

import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX.Base (raw)
import qualified Text.LaTeX.Packages.AMSMath as LaTeX
import qualified Text.LaTeX.Base.Commands as LaTeX

import Data.Monoid



prime :: LaTeXC l => l -> l
prime = (<>raw"'")

