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
{-# LANGUAGE CPP                 #-}

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


type MathsInfix = ∀ γ σ .
      CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)
       -> CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)
              -> CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)

atom :: l -> CAS' γ s² s¹ (SymbolD σ l)
atom = Symbol . StringSymbol

encapsulation :: l -> l
              -> (CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l))
              -> (CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l))
encapsulation l r = Function $ Encapsulation False True l r

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

infixl 6 ±, ∓, ⊕, +..+
(±), (∓), (⊕), (+..+) :: MathsInfix
(±) = opL 6 LaTeX.pm
(∓) = opL 6 LaTeX.mp
(⊕) = opL' 6 LaTeX.oplus
(+..+) = opL 6 $ raw"+\\ldots+"

infixl 7 ×, ∘, ⊗, *..*
(×), (⊗), (∘), (*..*) :: MathsInfix
(×) = opL' 7 LaTeX.times
(⊗) = opL' 7 LaTeX.otimes
(∘) = opL' 7 LaTeX.circ
(*..*) = opL 7 $ raw"{\\cdot\\ldots\\cdot}"

infixr 3 ∧, ∨
(∧), (∨) :: MathsInfix
(∧) = opR 3 $ raw"\\wedge{}"
(∨) = opR 3 $ raw"\\vee{}"

infixr 5 ⸪, -→, ↪
(⸪), (-→), (↪) :: MathsInfix
(⸪) = opR 5 ":"
(-→) = opR 5 LaTeX.to
(↪) = opR 5 $ raw"\\hookrightarrow{}"

infixl 7 °
infixr 9 ◝, ◝⁀
infixr 9 ◞, ⁀
infixl 8 |◞, |◝, |◞◝
infixl 8 ◞◝
(°), (⁀), (◝), (◝⁀), (◞), (|◞) :: MathsInfix
f°x = opL 7 mempty f (encapsulation (raw"\\left(") (raw"\\right)") x)
(⁀) = opL 9 mempty
l◝⁀s = opR 9 mempty l $ encapsulation (raw"\\left(") (raw"\\right)^") s
l◝s = Operator (Infix (Hs.Fixity 9 Hs.InfixR) mempty)
             l (encapsulation (raw "^{") (raw "}") s)
l◞s = Operator (Infix (Hs.Fixity 9 Hs.InfixR) mempty)
             l (encapsulation (raw "_{") (raw "}") s)
l◞◝(s,p) = Operator (Infix (Hs.Fixity 9 Hs.InfixR) mempty)
             l
           $ Operator (Infix (Hs.Fixity 9 Hs.InfixR) mempty)
                   (encapsulation (raw "_{") (raw "}") s)
                   (encapsulation (raw "^{") (raw "}") p)
l|◝s = Operator (Infix (Hs.Fixity 8 Hs.InfixR) mempty)
             (encapsulation (raw "left.") (raw "right|") l)
             (encapsulation (raw "^{") (raw "}") s)
l|◞s = Operator (Infix (Hs.Fixity 8 Hs.InfixR) mempty)
             (encapsulation (raw "left.") (raw "right|") l)
             (encapsulation (raw "_{") (raw "}") s)
l|◞◝(s,p) = Operator (Infix (Hs.Fixity 8 Hs.InfixR) mempty)
             (encapsulation (raw "left.") (raw "right|") l)
           $ Operator (Infix (Hs.Fixity 8 Hs.InfixR) mempty)
                   (encapsulation (raw "_{") (raw "}") s)
                   (encapsulation (raw "^{") (raw "}") p)


makeOperatorCaste "implicationOperators"
                  (''MathsInfix, ''LaTeX)
                  (Fixity 1 InfixL)
                  True
                  [ ("==>", [e|raw"\\Longrightarrow "|])
                  , ("<==", [e|raw"\\Longleftarrow "|])
                  , ("<=>", [e|raw"\\Longleftrightarrow "|])
                  ]

makeOperatorCaste "relationOperators"
                  (''MathsInfix, ''LaTeX)
                  (Fixity 4 InfixL)
                  True
                  [ ("⩵", [e|""LaTeX.=:""|])
#if __GLASGOW_HASKELL__ > 802
                  , ("⸪=", [e|raw"{:=}"|])
                  , ("=⸪", [e|raw"{=:}"|])
#endif
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

del, nabla, infty :: (SymbolClass σ, SCConstraint σ LaTeX)
          => CAS' γ s² s¹ (SymbolD σ LaTeX)
del = atom LaTeX.partial
nabla = atom $ raw "\\nabla{}\\!"
infty = atom LaTeX.infty



newtype Integrand γ s² s¹ s⁰ = Integrand { getIntgrand :: CAS' γ s² s¹ s⁰ }

d :: LaTeXC l => CAS' γ (Infix l) (Encapsulation l) s⁰
              -> CAS' γ (Infix l) (Encapsulation l) s⁰
              -> Integrand γ (Infix l) (Encapsulation l) s⁰
d x f = Integrand $ opR 7 (raw "\\ ") x f

infixr 8 ∫, ◞∫, ◞∮, ∑, ◞∑, ∏, ◞∏

(∫) :: LaTeXC l
  => ( CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
     , CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l) )
              -> Integrand γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
(l,r)∫Integrand i
    = Operator (Infix (Hs.Fixity 7 Hs.InfixR) $ LaTeX.mathrm "d")
             (Operator (Infix (Hs.Fixity 9 Hs.InfixN) $ raw "^")
                (encapsulation (raw "\\int\\limits_{") (raw "}") l)
                (encapsulation (raw "{") (raw "}") r) )
             i

(◞∫) :: LaTeXC l
  => CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> Integrand γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
ω◞∫Integrand i
    = Operator (Infix (Hs.Fixity 7 Hs.InfixR) $ LaTeX.mathrm "d")
             (encapsulation (raw "\\int_{") (raw "}\\!\\!\\!") ω)
             i

(◞∮) :: LaTeXC l
  => CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> Integrand γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
ω◞∮Integrand i
    = Operator (Infix (Hs.Fixity 7 Hs.InfixR) $ LaTeX.mathrm "d")
             (encapsulation (raw "\\oint_{") (raw "}\\!\\!\\!") ω)
             i

(∑) :: LaTeXC l
  => ( CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
     , CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l) )
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
(l,r)∑m
    = Operator (Infix (Hs.Fixity 7 Hs.InfixR) $ raw" ")
             (Operator (Infix (Hs.Fixity 9 Hs.InfixN) $ raw "^")
                (encapsulation (raw "\\sum_{") (raw "}") l)
                (encapsulation (raw "{") (raw "}") r) )
             m

(◞∑) :: LaTeXC l
  => CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
ω◞∑m
    = Operator (Infix (Hs.Fixity 7 Hs.InfixR) $ raw " ")
             (encapsulation (raw "\\sum_{") (raw "}") ω)
             m

(∏) :: LaTeXC l
  => ( CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
     , CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l) )
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
(l,r)∏m
    = Operator (Infix (Hs.Fixity 7 Hs.InfixR) $ raw" ")
             (Operator (Infix (Hs.Fixity 9 Hs.InfixN) $ raw "^")
                (encapsulation (raw "\\prod_{") (raw "}") l)
                (encapsulation (raw "{") (raw "}") r) )
             m

(◞∏) :: LaTeXC l
  => CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
              -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
ω◞∏m
    = Operator (Infix (Hs.Fixity 7 Hs.InfixR) $ raw " ")
             (encapsulation (raw "\\prod_{") (raw "}") ω)
             m



makeOperatorCaste "juxtapositionOperators"
                  (''MathsInfix, ''LaTeX)
                  (Fixity 0 InfixR)
                  True
                  [ ("␣", [e|raw"\\ "|])
                  , ("...", [e|raw"{\\ldots}"|])
#if __GLASGOW_HASKELL__ > 802
                  , ("،", [e|raw","|])
                  , ("،..،", [e|raw",\\ldots,"|])
#endif
                  ]



set :: LaTeXC l
  => CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
    -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
set = encapsulation (raw"\\left\\{") (raw"\\right\\}")





toMathLaTeX' :: ∀ σ l . (LaTeXC l, SymbolClass σ, SCConstraint σ LaTeX)
     => CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX) -> l
toMathLaTeX' = fromLaTeX . toMathLaTeX

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

