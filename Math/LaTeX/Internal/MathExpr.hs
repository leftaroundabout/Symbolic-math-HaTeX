-- |
-- Module      : Math.LaTeX.Internal.MathExpr
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>7 extensions
-- 

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Math.LaTeX.Internal.MathExpr where

import qualified Text.LaTeX as LaTeX
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX)
import qualified Text.LaTeX.Base.Class as LaTeX
import qualified Text.LaTeX.Base.Types as LaTeX
import qualified Text.LaTeX.Base.Commands as LaTeX
import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv))
import qualified Text.LaTeX.Packages.AMSMath as LaTeX
import qualified Text.LaTeX.Packages.AMSFonts as LaTeX

import CAS.Dumb
import CAS.Dumb.Tree
import CAS.Dumb.Symbols
import CAS.Dumb.LaTeX.Symbols

import Data.Foldable (fold)
import Data.Monoid ((<>))

import qualified Language.Haskell.TH.Syntax as Hs

type MathsInfix = ∀ γ s⁰ l . LaTeXC l
   => CAS' γ (Infix l) (Encapsulation l) s⁰ -> CAS' γ (Infix l) (Encapsulation l) s⁰
              -> CAS' γ (Infix l) (Encapsulation l) s⁰

opL :: LaTeXC l => Int -> l
    -> CAS' γ (Infix l) (Encapsulation l) s⁰ -> CAS' γ (Infix l) (Encapsulation l) s⁰
              -> CAS' γ (Infix l) (Encapsulation l) s⁰
opL fxty iop = symbolInfix (Infix (Hs.Fixity fxty Hs.InfixL) iop)

infixl 6 ±, ∓
(±), (∓) :: MathsInfix
(±) = opL 6 LaTeX.pm
(∓) = opL 6 LaTeX.mp


toMathLaTeX :: ∀ σ l . (LaTeXC l, Num l, SymbolClass σ, SCConstraint σ l)
                => CAS (Infix l) (Encapsulation l) (SymbolD σ l) -> l
toMathLaTeX = renderSymbolExpression (AtLHS $ Hs.Fixity 0 Hs.InfixL) ρ
 where ρ dop lctxt (StringSymbol sym) rctxt
           = showLParen dop $ flip (foldr (<>)) lctxt $ foldl (<>) sym rctxt
       ρ dop lctxt (NatSymbol n) rctxt
           = showLParen dop $ flip (foldr (<>)) lctxt $ foldl (<>) (fromInteger n) rctxt
       ρ dop lctxt (PrimitiveSymbol c) rctxt
           = case fromCharSymbol ([]::[σ]) of
              fcs -> showLParen dop $ flip (foldr (<>)) lctxt $ foldl (<>) (fcs c) rctxt

showLParen :: LaTeXC l => Bool -> l -> l
showLParen True  = LaTeX.autoParens
showLParen False = id

