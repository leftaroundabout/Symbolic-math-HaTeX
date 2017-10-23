-- |
-- Module      : CAS.Dumb.LaTeX.Symbols
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- Orphan instances, allowing to construct CAS syntax trees
-- with LaTeX symbols.

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP                  #-}

module CAS.Dumb.LaTeX.Symbols () where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSFonts

import qualified Data.Text as Txt
import Data.String (IsString(..))
import Data.Char (isAlpha, isUpper, isLower)
import Data.Tuple (swap)

import Data.Ratio (denominator, numerator)
import Numeric.Literals.Decimal

import qualified Data.HashMap.Strict as Map
import Data.Hashable

import Control.Monad

import qualified Language.Haskell.TH as Hs


instance ASCIISymbols LaTeX where
  fromASCIISymbol c
   | isAlpha c  = fromString [c]
  toASCIISymbols (TeXRaw s) = Txt.unpack s

instance UnicodeSymbols LaTeX where
  fromUnicodeSymbol c
   | Just lc <- Map.lookup c mappingFromUnicode  = lc
   | otherwise  = error $ "Unicode symbol '"++[c]++"' not supported in LaTeX expressions."
  toUnicodeSymbols lc
   | Just c <- Map.lookup lc mappingToUnicode    = [c]
   | lc==mempty  = ""
   | otherwise   = "《"++Txt.unpack(render lc)++"》"
  
mappingFromUnicode :: Map.HashMap Char LaTeX
mappingToUnicode :: Map.HashMap LaTeX Char
InvertibleMap mappingFromUnicode mappingToUnicode
   = mapToLaTeXWith id     "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧"
                           "abcdefghijklmnopqrstuvwxyz"
 <|> mapToLaTeXWith mathbf ['𝐚'..'𝐳']
                           ['a'..'z']
 <|> mapToLaTeXWith id     ['𝐴'..'𝑍']
                           ['A'..'Z']
 <|> mapToLaTeXWith mathbf ['𝐀'..'𝐙']
                           ['A'..'Z']
 <|> mapToLaTeXWith mathbb "ℂℍℚℝℤ"
                           "CHQRZ"
 <|> mapToLaTeXWith mathcal ['𝓐'..'𝓩']
                            ['A'..'Z']
 <|> mapToLaTeXWith mathfrak "𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜"
                             "ABCDEFGHIJKLMNOPQRSTUVWXY"
 <|> fromAssocList (zip
           ['α',  'β', 'γ',  'δ',  'ε',       'ζ', 'η','θ',  'ϑ',     'ι', 'κ',  'λ'   ]
           [alpha,beta,gamma,delta,varepsilon,zeta,eta,theta,vartheta,iota,kappa,lambda])
 <|> fromAssocList (zip
           ['μ','ν','ξ','π','ρ','ϱ',   'σ',  'ς',     'τ','υ',    'ϕ','φ',   'χ','ψ', 'ω' ]
           [mu, nu, xi, pi, rho,varrho,sigma,varsigma,tau,upsilon,phi,varphi,chi,psi,omega])
 <|> fromAssocList (zip
           ['Γ',   'Δ',   'Θ',   'Λ',    'Ξ','Π','Σ',   'Υ',     'Φ', 'Ψ', 'Ω'   ]
           [gammau,deltau,thetau,lambdau,xiu,piu,sigmau,upsilonu,phiu,psiu,omegau])
 <|> fromAssocList (zip
           ['+', '-', '*',           '±',         '∓'        ]
           ["+", "-", raw"{\\cdot}", raw"{\\pm}", raw"{\\mp}"])

remapWith :: (a->b) -> [a] -> [a] -> [(a, b)]
remapWith f = zipWith (\lc rc -> (lc, f rc))

mapToLaTeXWith :: (LaTeX->LaTeX) -> [Char] -> [Char] -> InvertibleMap Char LaTeX
mapToLaTeXWith f l r = fromAssocList $ remapWith (f . fromString . pure) l r



data InvertibleMap a b = InvertibleMap {
      fwdMapping :: Map.HashMap a b
    , revMapping :: Map.HashMap b a
    }

fromAssocList :: (Hashable a, Hashable b, Eq a, Eq b)
                 => [(a,b)] -> InvertibleMap a b
fromAssocList assocs = InvertibleMap (Map.fromList assocs) (Map.fromList $ map swap assocs)

infixl 3 <|>
(<|>) :: (Hashable a, Hashable b, Eq a, Eq b)
                 => InvertibleMap a b -> InvertibleMap a b -> InvertibleMap a b
InvertibleMap af ar<|>InvertibleMap bf br
   = InvertibleMap (Map.union af bf) (Map.union ar br)

encapsulation :: l -> l
              -> (CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l))
              -> (CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l))
encapsulation l r = Function $ Encapsulation False True l r

latexFunction :: LaTeXC l
              => Text
              -> (CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l))
              -> (CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l))
latexFunction f = Function $ Encapsulation True False (raw $ f<>"{") (raw "}")

instance ∀ σ γ . (SymbolClass σ, SCConstraint σ LaTeX)
          => Num (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) where
  fromInteger n
   | n<0        = negate . fromInteger $ -n
   | otherwise  = Symbol $ NatSymbol n
  (+) = chainableInfixL (==plusOp) plusOp
   where fcs = fromCharSymbol ([]::[σ])
         plusOp = Infix (Hs.Fixity 6 Hs.InfixL) $ fcs '+'
  (*) = chainableInfixL (==mulOp) mulOp
   where fcs = fromCharSymbol ([]::[σ])
         mulOp = Infix (Hs.Fixity 7 Hs.InfixL) $ fcs '*'
  (-) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) $ fcs '-')
   where fcs = fromCharSymbol ([]::[σ])
  abs = encapsulation (raw "\\left|") (raw "\\right|")
  signum = latexFunction "\\signum"
  negate = Operator (Infix (Hs.Fixity 6 Hs.InfixL) $ fcs '-')
             . Symbol $ StringSymbol mempty
   where fcs = fromCharSymbol ([]::[σ])

instance ∀ σ γ . (SymbolClass σ, SCConstraint σ LaTeX)
     => Fractional (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) where
  fromRational n = case fromRational n of
     n:%d -> fromIntegral n / fromIntegral d
     Scientific pc acs e -> let m = Symbol (StringSymbol . fromString
                                     $ show pc++
                                       if null acs then ""
                                                   else "."++(show=<<acs))
                            in if e==0 then m
                                       else m * 10**fromIntegral e
  a / b = Operator (Infix (Hs.Fixity 8 Hs.InfixL) mempty)
             (encapsulation (raw "\\frac{") (raw "}") a)
             (encapsulation (raw       "{") (raw "}") b)


instance ∀ σ γ . (SymbolClass σ, SCConstraint σ LaTeX)
     => Floating (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) where
  pi = Symbol $ StringSymbol pi_
  sqrt = encapsulation (raw "\\sqrt{") (raw "}")
  a ** b = Operator (Infix (Hs.Fixity 8 Hs.InfixR) mempty)
             a (encapsulation (raw "^{") (raw "}") b)
  logBase b a = Operator (Infix (Hs.Fixity 10 Hs.InfixL) mempty)
                  (encapsulation (raw "\\log_{") (raw "}") b) a
  exp = latexFunction "\\exp"
  log = latexFunction "\\log"
  sin = latexFunction "\\sin"
  cos = latexFunction "\\cos"
  tan = latexFunction "\\tan"
  asin = latexFunction "\\asin"
  acos = latexFunction "\\acos"
  atan = latexFunction "\\atan"
  sinh = latexFunction "\\sinh"
  cosh = latexFunction "\\cosh"
  tanh = latexFunction "\\tanh"
  asinh = latexFunction "\\asinh"
  acosh = latexFunction "\\acosh"
  atanh = latexFunction "\\atanh"



instance Eq (Encapsulation LaTeX) where
  Encapsulation _ _ l r == Encapsulation _ _ l' r'
         = l==l' && r==r'

