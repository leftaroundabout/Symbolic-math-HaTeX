-- |
-- Module      : CAS.Dumb.LaTeX.Symbols
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
-- Orphan instances, allowing to construct CAS syntax trees
-- with LaTeX symbols.

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE CPP                  #-}

module CAS.Dumb.LaTeX.Symbols ( fixateLaTeXAlgebraEncaps
                              , LaTeXMathEncapsulation(..) ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols hiding (Negation, Reciprocal)

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
import Control.Arrow (second)

import qualified Language.Haskell.TH as Hs


data LaTeXMathEncapsulation
       = Negation | Reciprocal | Subscript | Superscript
        | StdMathsFunc StdMathsFunc
 deriving (Eq, Show)

data StdMathsFunc
   = Abs
   | ConventionalFunction Text
 deriving (Eq, Show)

type instance SpecialEncapsulation LaTeX = LaTeXMathEncapsulation

instance RenderableEncapsulations LaTeX where
  fixateAlgebraEncaps = fixateShowAlgebraEncaps

showMagic :: Text -> LaTeX
showMagic s = raw $ "｛"<>s<>"｝"
matchShowMagic :: LaTeX -> Maybe Text
matchShowMagic e = case render e of
    s' | "｛"`Txt.isPrefixOf`s'
       , "｝"`Txt.isSuffixOf`s'  -> Just $ Txt.drop 1
                                         $ Txt.dropEnd 1 s'
    _           -> Nothing

fixateShowAlgebraEncaps :: ∀ σ γ . (SymbolClass σ, SCConstraint σ LaTeX)
         => CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)
          -> CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)
fixateShowAlgebraEncaps (OperatorChain x
                         ((o,Function (SpecialEncapsulation ι) z):ys))
     | (Infix (Hs.Fixity 6 Hs.InfixL) addSym', Negation) <- (o,ι)
     , addSym' == addSym
           = case fixateShowAlgebraEncaps $ OperatorChain x ys of
               x' -> Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-") x' z'
     | (Infix (Hs.Fixity 7 Hs.InfixL) mulSym', Reciprocal) <- (o,ι)
     , mulSym' == mulSym
           = case fixateShowAlgebraEncaps $ OperatorChain x ys of
               x' -> Operator (Infix (Hs.Fixity 7 Hs.InfixL) $ showMagic "/") x' z'
   where [addSym, mulSym] = fromCharSymbol ([]::[σ]) <$> "+*" :: [LaTeX]
         z' = fixateShowAlgebraEncaps z
fixateShowAlgebraEncaps (OperatorChain x []) = fixateShowAlgebraEncaps x
fixateShowAlgebraEncaps (OperatorChain x ((o@(Infix (Hs.Fixity _ Hs.InfixL) _), z):ys))
      = Operator o (fixateShowAlgebraEncaps $ OperatorChain x ys) (fixateShowAlgebraEncaps z)
fixateShowAlgebraEncaps (Operator o x (Function (SpecialEncapsulation ι) y))
     | (Infix (Hs.Fixity 6 Hs.InfixL) addSym', Negation) <- (o,ι)
     , addSym' == addSym
           = Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-") x' y'
     | (Infix (Hs.Fixity 7 Hs.InfixL) mulSym', Reciprocal) <- (o,ι)
     , mulSym' == mulSym
           = Operator (Infix (Hs.Fixity 7 Hs.InfixL) $ showMagic "/") x' y'
     | (Infix fxty catSym', Superscript) <- (o,ι)
     , catSym' == mempty
           = Operator (Infix fxty $ showMagic "◝") x' y'
     | (Infix fxty catSym', Subscript) <- (o,ι)
     , catSym' == mempty
           = Operator (Infix fxty $ showMagic "◞") x' y'
   where [addSym, mulSym] = fromCharSymbol ([]::[σ]) <$> "+*" :: [LaTeX]
         [x',y'] = fixateShowAlgebraEncaps<$>[x,y]
fixateShowAlgebraEncaps (Function (SpecialEncapsulation Negation) e)
            = Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-")
                (Symbol $ StringSymbol " ") $ fixateShowAlgebraEncaps e
fixateShowAlgebraEncaps (Function (SpecialEncapsulation Reciprocal) e)
            = Operator (Infix (Hs.Fixity 7 Hs.InfixL) $ showMagic "/")
               (Symbol $ NatSymbol 1)
               (fixateShowAlgebraEncaps e)
fixateShowAlgebraEncaps (Function (SpecialEncapsulation Superscript) e)
            = Operator (Infix (Hs.Fixity 7 Hs.InfixL) $ showMagic "◝")
               (Symbol $ StringSymbol "\"\"")
               (fixateShowAlgebraEncaps e)
fixateShowAlgebraEncaps (Function (SpecialEncapsulation Subscript) e)
            = Operator (Infix (Hs.Fixity 7 Hs.InfixL) $ showMagic "◞")
               (Symbol $ StringSymbol "\"\"")
               (fixateShowAlgebraEncaps e)
fixateShowAlgebraEncaps (StdMathFn Abs e)
            = haskellFunction "abs" $ fixateShowAlgebraEncaps e
fixateShowAlgebraEncaps (ConventionalMathFn f e)
            = haskellFunction f $ fixateShowAlgebraEncaps e
fixateShowAlgebraEncaps (Function f e) = Function f $ fixateShowAlgebraEncaps e
fixateShowAlgebraEncaps (Operator o x y)
        = Operator o (fixateShowAlgebraEncaps x) (fixateShowAlgebraEncaps y)
fixateShowAlgebraEncaps (OperatorChain x₀ oys)
        = OperatorChain (fixateShowAlgebraEncaps x₀) (second fixateShowAlgebraEncaps <$> oys)
fixateShowAlgebraEncaps e = e

fixateLaTeXAlgebraEncaps :: ∀ σ γ . (SymbolClass σ, SCConstraint σ LaTeX)
         => CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)
          -> CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)
fixateLaTeXAlgebraEncaps (OperatorChain x
                         ((o,Function (SpecialEncapsulation ι) z):ys))
     | (Infix (Hs.Fixity 6 Hs.InfixL) addSym', Negation) <- (o,ι)
     , addSym' == addSym
           = case fixateLaTeXAlgebraEncaps $ OperatorChain x ys of
               x' -> Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-") x' z'
     | (Infix (Hs.Fixity 7 Hs.InfixL) mulSym', Reciprocal) <- (o,ι)
     , mulSym' == mulSym
           = case fixateLaTeXAlgebraEncaps $ OperatorChain x ys of
               x' -> Operator (Infix (Hs.Fixity 8 Hs.InfixL) mempty)
                  (encapsulation (raw "\\frac{") (raw "}") x')
                  (encapsulation (raw       "{") (raw "}") z')
   where [addSym, mulSym] = fromCharSymbol ([]::[σ]) <$> "+*" :: [LaTeX]
         z' = fixateLaTeXAlgebraEncaps z
fixateLaTeXAlgebraEncaps (OperatorChain x []) = fixateLaTeXAlgebraEncaps x
fixateLaTeXAlgebraEncaps (OperatorChain x ((o@(Infix (Hs.Fixity _ Hs.InfixL) _), z):ys))
      = Operator o (fixateLaTeXAlgebraEncaps $ OperatorChain x ys) (fixateLaTeXAlgebraEncaps z)
fixateLaTeXAlgebraEncaps (Operator o x (Function (SpecialEncapsulation ι) y))
     | (Infix (Hs.Fixity 6 Hs.InfixL) addSym', Negation) <- (o,ι)
     , addSym' == addSym
           = Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-") x' y'
     | (Infix (Hs.Fixity 7 Hs.InfixL) mulSym', Reciprocal) <- (o,ι)
     , mulSym' == mulSym
           = Operator (Infix (Hs.Fixity 8 Hs.InfixL) mempty)
                  (encapsulation (raw "\\frac{") (raw "}") x')
                  (encapsulation (raw       "{") (raw "}") y')
     | (Infix fxty catSym', Superscript) <- (o,ι)
     , catSym' == mempty
           = Operator (Infix fxty (raw "^"))
                  x'
                  (encapsulation (raw       "{") (raw "}") y')
     | (Infix fxty catSym', Subscript) <- (o,ι)
     , catSym' == mempty
           = Operator (Infix fxty (raw "_"))
                  x'
                  (encapsulation (raw       "{") (raw "}") y')
   where [addSym, mulSym] = fromCharSymbol ([]::[σ]) <$> "+*" :: [LaTeX]
         [x',y'] = fixateLaTeXAlgebraEncaps<$>[x,y]
fixateLaTeXAlgebraEncaps (Function (SpecialEncapsulation Negation) e)
            = Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-")
                (Symbol $ StringSymbol " ") $ fixateLaTeXAlgebraEncaps e
fixateLaTeXAlgebraEncaps (Function (SpecialEncapsulation Reciprocal) e)
            = Operator (Infix (Hs.Fixity 8 Hs.InfixL) mempty)
               (encapsulation (raw "\\frac{") (raw "}") . Symbol $ NatSymbol 1)
               (encapsulation (raw       "{") (raw "}") $ fixateLaTeXAlgebraEncaps e)
fixateLaTeXAlgebraEncaps (Function (SpecialEncapsulation Superscript) e)
            = encapsulation (raw "{}^{") (raw "}") $ fixateLaTeXAlgebraEncaps e
fixateLaTeXAlgebraEncaps (Function (SpecialEncapsulation Subscript) e)
            = encapsulation (raw "{}_{") (raw "}") $ fixateLaTeXAlgebraEncaps e
fixateLaTeXAlgebraEncaps (StdMathFn Abs e)
            = encapsulation (raw "\\left|") (raw "\\right|") $ fixateLaTeXAlgebraEncaps e
fixateLaTeXAlgebraEncaps (ConventionalMathFn f e)
            = latexFunction ("\\"<>f) $ fixateLaTeXAlgebraEncaps e
fixateLaTeXAlgebraEncaps (Function f e) = Function f $ fixateLaTeXAlgebraEncaps e
fixateLaTeXAlgebraEncaps (Operator o x y)
        = Operator o (fixateLaTeXAlgebraEncaps x) (fixateLaTeXAlgebraEncaps y)
fixateLaTeXAlgebraEncaps (OperatorChain x₀ oys)
        = OperatorChain (fixateLaTeXAlgebraEncaps x₀) (second fixateLaTeXAlgebraEncaps <$> oys)
fixateLaTeXAlgebraEncaps e = e


instance ASCIISymbols LaTeX where
  fromASCIISymbol c
   | Just lc <- Map.lookup c mappingFromASCII  = lc
   | otherwise  = error $ "ASCII symbol '"++[c]++"' not supported in LaTeX expressions."
  toASCIISymbols lc
   | Just c <- Map.lookup lc mappingToASCII    = [c]
   | lc==mempty  = ""
   | Just s' <- matchShowMagic lc  = Txt.unpack s'
   | otherwise   = "《"++Txt.unpack(render lc)++"》"

mappingFromASCII :: Map.HashMap Char LaTeX
mappingToASCII :: Map.HashMap LaTeX Char
InvertibleMap mappingFromASCII mappingToASCII
   = mapToLaTeXWith id     ['a'..'z']
                           ['a'..'z']
 <|> mapToLaTeXWith id     ['A'..'Z']
                           ['A'..'Z']
 <|> fromAssocList (zip
           ['+', '-', '*']
           ["+", "-", raw"{\\cdot}"])

instance UnicodeSymbols LaTeX where
  fromUnicodeSymbol c
   | Just lc <- Map.lookup c mappingFromUnicode  = lc
   | otherwise  = error $ "Unicode symbol '"++[c]++"' not supported in LaTeX expressions."
  toUnicodeSymbols lc
   | Just c <- Map.lookup lc mappingToUnicode    = [c]
   | lc==mempty  = ""
   | Just s' <- matchShowMagic lc  = Txt.unpack s'
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
 <|> mapToLaTeXWith mathbb "ℂℍℚℝℤℕ"
                           "CHQRZN"
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

haskellFunction :: l ~ LaTeX
              => Text
              -> (CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l))
              -> (CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l))
haskellFunction f
    = Function $ Encapsulation True False (showMagic $ f<>" ") mempty

pattern StdMathFn :: 
           StdMathsFunc -> (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX))
                        -> (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX))
pattern StdMathFn f e = Function (SpecialEncapsulation (StdMathsFunc f)) e

pattern ConventionalMathFn :: 
           Text -> (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX))
                        -> (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX))
pattern ConventionalMathFn f e = StdMathFn (ConventionalFunction f) e

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
  abs = StdMathFn Abs
  signum = latexFunction "\\signum"
  negate = Function $ SpecialEncapsulation Negation

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
  recip = Function $ SpecialEncapsulation Reciprocal


instance ∀ σ γ . (SymbolClass σ, SCConstraint σ LaTeX)
     => Floating (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) where
  pi = Symbol $ StringSymbol pi_
  sqrt = encapsulation (raw "\\sqrt{") (raw "}")
  a ** b = Operator (Infix (Hs.Fixity 8 Hs.InfixR) mempty)
             a (Function (SpecialEncapsulation Superscript) b)
  logBase b a = Operator (Infix (Hs.Fixity 10 Hs.InfixL) mempty)
                  (encapsulation (raw "\\log_{") (raw "}") b) a
  exp = ConventionalMathFn "exp"
  log = ConventionalMathFn "log"
  sin = ConventionalMathFn "sin"
  cos = ConventionalMathFn "cos"
  tan = ConventionalMathFn "tan"
  asin = latexFunction "\\arcsin"
  acos = latexFunction "\\arccos"
  atan = latexFunction "\\arctan"
  sinh = ConventionalMathFn "sinh"
  cosh = ConventionalMathFn "cosh"
  tanh = ConventionalMathFn "tanh"
  asinh = latexFunction "\\operatorname{arsinh}"
  acosh = latexFunction "\\operatorname{arcosh}"
  atanh = latexFunction "\\operatorname{artanh}"



instance Eq (Encapsulation LaTeX) where
  Encapsulation _ _ l r == Encapsulation _ _ l' r'
         = l==l' && r==r'
  SpecialEncapsulation e == SpecialEncapsulation e' = e==e'
  _ == _ = False

