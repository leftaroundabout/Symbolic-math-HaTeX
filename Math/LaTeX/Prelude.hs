-- |
-- Module      : Math.LaTeX.Prelude
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : requires GHC>7 extensions
-- 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Math.LaTeX.Prelude (
     LaTeXMath
   -- * Primitive symbols
   , LaTeXSymbol
   -- ** Unicode literals
   , module CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps
   -- $unicodeLiterals
   , LaTeXMath__MathLatin_RomanGreek__BopomofoGaps
   -- ** Custom symbol-literals
   -- $nonunicodeLiterals

   -- ** Symbol modifiers
   , (%$>), prime
   , LaTeX.dot, LaTeX.ddot, LaTeX.bar, LaTeX.hat
   , LaTeX.vec, LaTeX.underline, LaTeX.tilde
   -- * Maths operators
   , (°), (⁀), (...)
#if __GLASGOW_HASKELL__ > 801
   , (،..،), (،), (⸪=), (=⸪)
#endif
   , (␣), (+..+), (*..*), (×), (⊗), (∘), factorial
   , (◝), (◝⁀), (◞), (◞◝), (|◞), (|◝), (|◞◝)
   , (⩵), (≡), (⩵!), (≠), (⪡), (⪢), (≤), (≥), (≪), (≫), (∝), (⟂), (∥), (₌₌)
   , (=→), (←=), (≈), (∼), (≃), (≅)
   , (⊂), (/⊂), (⊆), (⊃), (⊇), (∋), (∌), (∈), (∉), (∩), (∪), (⊎), (∖), (-\-), (⧵), (⸪), (⊕)
   , (∀:), (∃:), (∄:)
   , (-→), (←-), (↦), (↪), (==>), (<==), (<=>), (∧), (∨)
   , (∫), (◞∫), (◞∮), d, (∑), (◞∑), (∏), (◞∏)
   , (⋃), (◞⋃), (⋂), (◞⋂), (⨄), (◞⨄)
   , del, nabla
   , (<.<), (≤.<), (<.≤), (≤.≤), (±), (∓), set, setCompr, tup, intv
   , infty, norm
   , nobreaks, matrix, cases
   -- * Algebraic manipulation
   -- #algebraManip
   , (&~~!), (&~~:), continueExpr, (&)
   , (&~:), (&~?), (&~!), (|->)
   -- * Use in documents
   , (Math.LaTeX.Prelude.$<>), (Math.LaTeX.Prelude.>$)
   , dmaths, maths, equations, dcalculation, toMathLaTeX
   ) where

import CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps hiding ((%$>))
import CAS.Dumb.Symbols
import Math.LaTeX.Internal.MathExpr
import Math.LaTeX.Internal.Display
import Math.LaTeX.StringLiterals

import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX.Base (raw, LaTeX)
import qualified Text.LaTeX.Packages.AMSMath as LaTeX
import qualified Text.LaTeX.Base.Commands as LaTeX

import Data.Monoid
import Data.Function ((&))

import CAS.Dumb.Tree

type LaTeXMath__MathLatin_RomanGreek__BopomofoGaps
   = CAS (Infix LaTeX) (Encapsulation LaTeX) (Symbol LaTeX)

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
--   Note: these versions of the '$<>' and '>$' operators have a signature that's
--   monomorphic to unicode symbol-literals.
--   (This restriction is to avoid ambiguous types when writing maths /without/ any
--   symbols in it, like simply embedding a fraction in inline text.) See 
--   [Custom literals](#nonunicodeLiteralsHowto) if this is a problem for you.
(>$) :: LaTeXC r
        => r -> LaTeXMath__MathLatin_RomanGreek__BopomofoGaps -> r
(>$) = (Math.LaTeX.Internal.Display.>$)

infixr 6 $<>
-- | Embed inline maths in a semigroup/monoidal chain of document-components.
--
-- @
--     "If "<>𝑎$<>" and "<>𝑏$<>" are the lengths of the legs and "<>𝑐$<>
--      " of the cathete of a right triangle, then "<>(𝑎◝2+𝑏◝2 ⩵ 𝑐◝2)$<>" holds."
-- @
--
--   This will be rendered as: If \(a\) and \(b\) are the lengths of the legs and \(c\)
--   of the cathete of a right triangle, then \(a^2+b^2=c^2\) holds.
($<>) :: LaTeXC r
        => LaTeXMath__MathLatin_RomanGreek__BopomofoGaps -> r -> r
($<>) = (Math.LaTeX.Internal.Display.$<>)

-- $unicodeLiterals
-- This module offers a “WYSiWYG” style, with italic Unicode math symbols
-- (@U+1d44e 𝑎@ - @U+1d467 𝑧@) coming out as standard italic symbols \(a\) - \(z\),
-- bold Unicode math symbols (@U+1d41a 𝐚@ - @U+1d433 𝐳@) coming out as bold
-- \(\mathbf{a}\) - \(\mathbf{z}\) and so on.
-- Greek letters can be used from the standard block
-- (@U+3b1 α@ → \(\alpha\) - @U+3c9 ω@ → \(\omega\)).
-- All of this also works for uppercase letters (it circumvents Haskell syntax
-- restrictions by using the @PatternSynonyms@ extension).
-- 
-- Upright (roman) symbols are not directly supported, but if you import
-- "Math.LaTeX.StringLiterals" they can be written as strings.
-- 
-- Example: @𝑎 + 𝐛 + 𝐶 + \"D\" + ε + Φ ∈ ℝ@ is rendered as
-- \(a + \mathbf{b} + C + \text{D} + \varepsilon + \Phi \in \mathbb{R}\).
-- 
-- The Bopomofo symbols here are not exported for use in documents but
-- for [Algebraic manipulation](#algebraManip).

-- $nonunicodeLiterals
-- If you prefer using instead e.g. ASCII letters @A@ - @z@ for simple symbols
-- \(A\) - \(z\), use this import list:
--
-- @
-- import Math.LaTeX.Prelude hiding ((>$), (<>$))
-- import "Math.LaTeX.Internal.Display" ((>$), (<>$))
-- import "CAS.Dumb.Symbols.ASCII"
-- @
-- 
-- We give no guarantee that this will work without name clashes or type ambiguities.

prime :: LaTeXC l => l -> l
prime = (<>raw"'")

infix 2 |->
(|->) :: CAS' γ s² s¹ s⁰ -> CAS' γ s² s¹ s⁰ -> Equality' γ s² s¹ s⁰
(|->) = (:=:)
