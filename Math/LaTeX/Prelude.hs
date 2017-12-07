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
     LaTeXMath
   -- * Use in documents
   , (Math.LaTeX.Prelude.>$), (Math.LaTeX.Prelude.$<>)
   , dmaths, maths, dcalculation, toMathLaTeX
   -- * Primitive symbols
   , module CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps
   -- ** Modifiers
   , (%$>), prime, LaTeX.bar, LaTeX.hat, LaTeX.vec, LaTeX.underline, LaTeX.tilde
   -- * Operators
   , (°), (⁀), (...)
#if __GLASGOW_HASKELL__ > 801
   , (،..،), (،), (⸪=), (=⸪)
#endif
   , (␣), (+..+), (*..*), (×), (⊗), (∘), factorial
   , (◝), (◝⁀), (◞), (◞◝), (|◞), (|◝), (|◞◝)
   , (⩵), (≡), (⩵!), (≠), (⪡), (⪢), (≤), (≥), (≪), (≫), (∝), (₌₌)
   , (=→), (≈), (∼)
   , (⊂), (/⊂), (⊆), (⊃), (⊇), (∋), (∌), (∈), (∉), (∩), (∪), (-\-), (⸪), (⊕)
   , (∀:), (∃:)
   , (-→), (↦), (↪), (==>), (<==), (<=>), (∧), (∨)
   , (∫), (◞∫), (◞∮), d, (∑), (◞∑), (∏), (◞∏), del, nabla
   , (<.<), (≤.<), (<.≤), (≤.≤), (±), (∓), set, tup, intv
   , infty, norm
   , nobreaks, matrix, cases
   -- * Algebraic manipulation
   , (&~~!), (&~~:), continueExpr, (&)
   , (&~:), (&~?), (&~!), (|->)
   ) where

import CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps hiding ((%$>))
import CAS.Dumb.Symbols
import Math.LaTeX.Internal.MathExpr
import Math.LaTeX.Internal.Display

import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX.Base (raw, LaTeX)
import qualified Text.LaTeX.Packages.AMSMath as LaTeX
import qualified Text.LaTeX.Base.Commands as LaTeX

import Data.Monoid
import Data.Function ((&))

import CAS.Dumb.Tree


-- | Mathematical expressions to be typeset in LaTeX.
--   Most of the functions in this library have more generic signatures, but
--   all can be used with this concrete type.
type LaTeXMath = CAS (Infix LaTeX) (Encapsulation LaTeX) (Symbol LaTeX)

infixl 1 >$
-- | Embed inline maths in a monadic chain of document-components. Space before
--   the math is included automatically.
--
-- @
--   do
--     \"If\">$𝑎;" and">$𝑏;" are the lengths of the legs and">$𝑐
--     " of the cathete of a right triangle, then">$ 𝑎◝2+𝑏◝2 ⩵ 𝑐◝2;" holds."
-- @
--
--   Note: this version of the operator has a simplified signature that's constrained
--   to symbols from "CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps".
--   Use 'Math.LaTeX.Internal.Display.>$' or 'toMathLaTeX' if you want to work with
--   e.g. "CAS.Dumb.Symbols.Unicode.ASCII" instead.
(>$) :: LaTeXC r
        => r -> LaTeXMath -> r
(>$) = (Math.LaTeX.Internal.Display.>$)

infixr 6 $<>
-- | Embed inline maths in a semigroup/monoidal chain of document-components.
--
-- @
--     "If "<>𝑎$<>" and "<>𝑏$<>" are the lengths of the legs and "<>𝑐$<>
--      " of the cathete of a right triangle, then "<>(𝑎◝2+𝑏◝2 ⩵ 𝑐◝2)$<>" holds."
-- @
--
--   Use 'Math.LaTeX.Internal.Display.$<>' to work with e.g. ASCII symbols
--   instead of "CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps".
($<>) :: LaTeXC r
        => LaTeXMath -> r -> r
($<>) = (Math.LaTeX.Internal.Display.$<>)

prime :: LaTeXC l => l -> l
prime = (<>raw"'")

infix 2 |->
(|->) :: CAS' γ s² s¹ s⁰ -> CAS' γ s² s¹ s⁰ -> Equality' γ s² s¹ s⁰
(|->) = (:=:)
