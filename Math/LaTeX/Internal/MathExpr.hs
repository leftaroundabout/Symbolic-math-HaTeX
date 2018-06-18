-- |
-- Module      : Math.LaTeX.Internal.MathExpr
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>7 extensions
-- 

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UnicodeSyntax          #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE CPP                    #-}

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

import Data.Foldable (fold, toList)
import Data.Semigroup
import qualified Data.List.NonEmpty as NE
import Data.Monoid hiding ((<>))
import Data.Void
import Data.String (IsString)

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

factorial :: LaTeXC l
         => CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
          -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
factorial n = Operator (Infix (Hs.Fixity 8 Hs.InfixR) $ raw"!")
                 n (Symbol $ StringSymbol mempty)


infixr 3 ∧, ∨
(∧), (∨) :: MathsInfix
(∧) = opR 3 $ raw"\\wedge{}"
(∨) = opR 3 $ raw"\\vee{}"

(∩), (∪), (-\-) :: MathsInfix
infixr 3 ∩
(∩) = opR' 3 LaTeX.cap
infixr 2 ∪
(∪) = opR' 2 LaTeX.cup
infixl 2 -\-
(-\-) = opL' 2 LaTeX.setminus

infixr 5 ⸪, -→, ↪
(⸪), (-→), (↪) :: MathsInfix
(⸪) = opR 5 ":"
(-→) = opR 5 LaTeX.to
(↪) = opR 5 $ raw"\\hookrightarrow{}"

infix 2 ∀:, ∃:
(∀:), (∃:) :: MathsInfix
(∀:) = opN 2 $ raw"\\ \\:\\forall{}"
(∃:) = opN 2 $ raw"\\ \\:\\exists{}"

infixl 7 °
infixr 9 ◝, ⁀, ◝⁀
infixr 9 ◞
infixl 8 |◞, |◝, |◞◝
infixl 8 ◞◝, ₌₌
(°), (⁀), (◝), (◝⁀), (◞), (|◞), (₌₌) :: MathsInfix
f°x = opL 7 mempty f (encapsulation (raw"\\left(") (raw"\\right)") x)
(⁀) = opR 9 mempty
l◝⁀s = opR 9 mempty l $ encapsulation (raw"\\left(") (raw"\\right)^") s
l◝s = Operator (Infix (Hs.Fixity 9 Hs.InfixR) mempty)
             l (encapsulation (raw "^{") (raw "}") s)
l◞s = Operator (Infix (Hs.Fixity 9 Hs.InfixR) mempty)
             l (encapsulation (raw "_{") (raw "}") s)
l₌₌s = Operator (Infix (Hs.Fixity 8 Hs.InfixR) mempty)
             (encapsulation (raw "\\underbrace{") (raw "}") l)
             (encapsulation (raw "_{") (raw "}") s)
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
#if __GLASGOW_HASKELL__ > 801
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
                  , ("∝", [e|LaTeX.propto""""|])
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

norm :: LaTeXC l => CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
            -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
norm = encapsulation (raw "\\left\\|") (raw "\\right\\|")
    


makeOperatorCaste "juxtapositionOperators"
                  (''MathsInfix, ''LaTeX)
                  (Fixity 0 InfixR)
                  True
                  [ ("␣", [e|raw"\\ "|])
                  , ("...", [e|raw"{\\ldots}"|])
#if __GLASGOW_HASKELL__ > 801
                  , ("،", [e|raw","|])
                  , ("،..،", [e|raw",\\ldots,"|])
#endif
                  ]


matrix :: LaTeXC l =>
        [[CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)]]
          -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
matrix [] = set (Symbol $ StringSymbol mempty)
matrix mlines = encapsulation (raw"\\begin{pmatrix}")
                                   (raw"\\end{pmatrix}")
     . OperatorChain le₀ . reverse
                $ (Infix (Hs.Fixity 0 Hs.InfixL) LaTeX.lnbk, ) <$> les
 where (le₀:les) = map (\(c₀:cs) -> OperatorChain c₀ . reverse
                          $ (Infix (Hs.Fixity 1 Hs.InfixL) $ raw"&", ) <$> cs) mlines

cases :: LaTeXC l
     => [(CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l), LaTeX)]
         -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
cases clauses = encapsulation (raw"\\begin{cases}") (raw"\\end{cases}")
           . OperatorChain cl₀ . reverse
                    $ (Infix (Hs.Fixity 0 Hs.InfixL) LaTeX.lnbk, ) <$> cls
 where (cl₀:cls) = map (\(r,co) -> Operator (Infix (Hs.Fixity 1 Hs.InfixL) $ raw"&")
                                     r $ Symbol (StringSymbol . LaTeX.comm1 "text"
                                                     $ fromLaTeX co)
                       ) clauses


set, tup, intv, nobreaks :: LaTeXC l
  => CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
    -> CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
set = encapsulation (raw"\\left\\{") (raw"\\right\\}")
tup = encapsulation (raw"\\left(") (raw"\\right)")
intv = encapsulation (raw"\\left[") (raw"\\right]")
nobreaks = encapsulation (raw"{") (raw"}")


infix 5 <.<, ≤.<, <.≤, ≤.≤
(<.<), (≤.<), (<.≤), (≤.≤) :: MathsInfix
l <.< r = encapsulation (raw"\\left]") (raw"\\right[") $ opN 0 (raw",") l r
l ≤.< r = encapsulation (raw"\\left[") (raw"\\right[") $ opN 0 (raw",") l r
l <.≤ r = encapsulation (raw"\\left]") (raw"\\right]") $ opN 0 (raw",") l r
l ≤.≤ r = encapsulation (raw"\\left[") (raw"\\right]") $ opN 0 (raw",") l r




toMathLaTeX' :: ∀ σ l . (LaTeXC l, SymbolClass σ, SCConstraint σ LaTeX)
     => CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX) -> l
toMathLaTeX' = fromLaTeX . toMathLaTeX

toMathLaTeX :: ∀ σ l . ( LaTeXC l, Num l, SymbolClass σ
                       , SCConstraint σ l, RenderableEncapsulations l )
                => CAS (Infix l) (Encapsulation l) (SymbolD σ l) -> l
toMathLaTeX = renderSymbolExpression (AtLHS $ Hs.Fixity 0 Hs.InfixL) ρ
              . fixateAlgebraEncaps
 where ρ dop lctxt (StringSymbol sym) rctxt
           = showLParen dop $ flip (foldr mappend) lctxt $ foldl mappend sym rctxt
       ρ dop lctxt (NatSymbol n) rctxt
           = showLParen dop $ flip (foldr mappend) lctxt $ foldl mappend (fromInteger n) rctxt
       ρ dop lctxt (PrimitiveSymbol c) rctxt
           = case fromCharSymbol ([]::[σ]) of
              fcs -> showLParen dop $ flip (foldr mappend) lctxt $ foldl mappend (fcs c) rctxt

showLParen :: LaTeXC l => Bool -> l -> l
showLParen True  = LaTeX.autoParens
showLParen False = id


instance (SymbolClass σ, SCConstraint σ LaTeX)
             => Semigroup (CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) where
  l<>r = sconcat $ l NE.:|[r]
  sconcat es = case don'tParenthesise <$> NE.toList es of
           [l,r] -> symbolInfix (Infix loosestFixity mempty) l r
           (l:rs) -> OperatorChain l [(Infix loosestFixity mempty, r) | r<-reverse rs]
   where loosestFixity = case foldr1 looser $ expressionFixity <$> es of
            Nothing -> Hs.Fixity 10 Hs.InfixR
            Just fxty -> fxty
         looser Nothing Nothing = Nothing
         looser (Just fxty) Nothing = Just fxty
         looser Nothing (Just fxty) = Just fxty 
         looser (Just (Hs.Fixity fxtyL fdL)) (Just (Hs.Fixity fxtyR fdR))
              | fxtyL > fxtyR    = Just $ Hs.Fixity fxtyR fdR
              | fxtyL < fxtyR
                || fdL == fdR    = Just $ Hs.Fixity fxtyL fdL
              | otherwise        = Just $ Hs.Fixity fxtyL Hs.InfixN

instance (SymbolClass σ, SCConstraint σ LaTeX)
             => Monoid (CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) where
  mempty = atom mempty
  mappend = (<>)
  mconcat [] = mempty
  mconcat (l : m) = sconcat $ l NE.:| m
instance ( SymbolClass σ, SCConstraint σ LaTeX
         , IsString (CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) )
             => LaTeXC (CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) where
  liftListL f = atom . f . map toMathLaTeX

instance ( γ ~ Void, s² ~ Infix LaTeX, s¹ ~ Encapsulation LaTeX, s⁰ ~ SymbolD σ LaTeX
         , SymbolClass σ, SCConstraint σ LaTeX )
    => LaTeX.Texy (CAS' γ s² s¹ s⁰) where
  texy = LaTeX.math . LaTeX.texy . toMathLaTeX
