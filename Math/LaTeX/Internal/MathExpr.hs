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
{-# LANGUAGE TemplateHaskell     #-}

module Math.LaTeX.Internal.MathExpr where

import qualified Text.LaTeX as LaTeX
import Text.LaTeX (raw)
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
import Math.LaTeX.Internal.OperatorGenerator

import Data.Foldable (fold)
import Data.Monoid ((<>))

import qualified Language.Haskell.TH.Syntax as Hs
import Language.Haskell.TH.Syntax (Fixity(..), FixityDirection(..))

type MathsInfix = ∀ γ s⁰ .
      CAS' γ (Infix LaTeX) (Encapsulation LaTeX) s⁰ -> CAS' γ (Infix LaTeX) (Encapsulation LaTeX) s⁰
              -> CAS' γ (Infix LaTeX) (Encapsulation LaTeX) s⁰

opL, opR, opN :: LaTeXC l => Int -> l
    -> CAS' γ (Infix l) (Encapsulation l) s⁰ -> CAS' γ (Infix l) (Encapsulation l) s⁰
              -> CAS' γ (Infix l) (Encapsulation l) s⁰
opL fxty iop = symbolInfix (Infix (Hs.Fixity fxty Hs.InfixL) iop)
opR fxty iop = symbolInfix (Infix (Hs.Fixity fxty Hs.InfixR) iop)
opN fxty iop = symbolInfix (Infix (Hs.Fixity fxty Hs.InfixN) iop)

opL', opR', opN' :: LaTeXC l => Int -> (l->l->l)
    -> CAS' γ (Infix l) (Encapsulation l) s⁰ -> CAS' γ (Infix l) (Encapsulation l) s⁰
              -> CAS' γ (Infix l) (Encapsulation l) s⁰
opL' fxty iop = symbolInfix (Infix (Hs.Fixity fxty Hs.InfixL) $ iop mempty mempty)
opR' fxty iop = symbolInfix (Infix (Hs.Fixity fxty Hs.InfixR) $ iop mempty mempty)
opN' fxty iop = symbolInfix (Infix (Hs.Fixity fxty Hs.InfixN) $ iop mempty mempty)

infixl 6 ±, ∓, ⊕
(±), (∓), (⊕) :: MathsInfix
(±) = opL 6 LaTeX.pm
(∓) = opL 6 LaTeX.mp
(⊕) = opL' 6 LaTeX.oplus

infixl 7 ×, ∘, ⊗
(×), (⊗), (∘) :: MathsInfix
(×) = opL' 7 LaTeX.times
(⊗) = opL' 7 LaTeX.otimes
(∘) = opL' 7 LaTeX.circ

infixr 3 ∧, ∨
(∧), (∨) :: MathsInfix
(∧) = opR 3 $ raw"\\wedge{}"
(∨) = opR 3 $ raw"\\vee{}"

infixr 5 ⸪, -→, ↪
(⸪), (-→), (↪) :: MathsInfix
(⸪) = opR 5 ":"
(-→) = opR 5 LaTeX.to
(↪) = opR 5 $ raw"\\hookrightarrow{}"

infixl 1 ==>, <=>, <==
(==>), (<=>), (<==) :: MathsInfix
(==>) = opL 1 $ raw"\\Longrightarrow "
(<==) = opL 1 $ raw"\\Longleftarrow "
(<=>) = opL 1 $ raw"\\Longleftrightarrow "



makeOperatorCaste "relationOperators"
                  (''MathsInfix, ''LaTeX)
                  (Fixity 4 InfixL)
                  True
                  [ ("⩵", [e|""LaTeX.=:""|])
                  , ("∶=", [e|raw"{:=}"|])
                  , ("⩵:", [e|raw"{=:}"|])
                  , ("≡", [e|raw" \\equiv "|])
                  , ("⩵!", [e|raw" \\overset{!}{=} "|])
                  , ("≠", [e|""LaTeX./=:""|])
                  , ("≈", [e|raw" \\approx "|])
                  , ("∼", [e|raw" \\sim "|])
                  , ("⪡", [e|""LaTeX.<:""|])
                  , ("⪢", [e|""LaTeX.>:""|])
                  , ("≤", [e|""LaTeX.<=:""|])
                  , ("≥", [e|""LaTeX.>=:""|])
                  , ("≪", [e|LaTeX.ll""""|])
                  , ("≫", [e|LaTeX.gg""""|])
                  , ("⊂", [e|LaTeX.subset""""|])
                  , ("/⊂", [e|raw" \\not\\subset "|])
                  , ("⊃", [e|LaTeX.supset""""|])
                  , ("⊆", [e|raw"\\subseteq{}"|])
                  , ("⊇", [e|raw"\\supseteq{}"|])
                  , ("∋", [e|LaTeX.ni""""|])
                  , ("∌", [e|raw"\\not\\ni{}"|])
                  , ("=→", [e|LaTeX.to|])
                  , ("∈", [e|LaTeX.in_""""|])
                  , ("∉", [e|raw"\\not\\in{}"|])
                  , ("↦", [e|LaTeX.mapsto|])
                  ]

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

