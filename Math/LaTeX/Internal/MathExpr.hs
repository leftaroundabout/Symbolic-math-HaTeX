-- |
-- Module      : Math.LaTeX.Internal.MathExpr
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 

{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE GADTs                            #-}
{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE UndecidableInstances             #-}
{-# LANGUAGE OverlappingInstances             #-}
{-# LANGUAGE PatternGuards                    #-}
{-# LANGUAGE TypeFamilies                     #-}
{-# LANGUAGE RankNTypes                       #-}
{-# LANGUAGE TupleSections                    #-}
{-# LANGUAGE RecordWildCards                  #-}

module Math.LaTeX.Internal.MathExpr where

import Math.LaTeX.RendConfig

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Data.Function
import Data.Functor.Contravariant

import Data.List
import Data.HList
import Data.HList.BasedUpon
import Data.Functor.FixedLength

import Data.Ratio
import Data.Complex(Complex(..))
import Data.Complex.Class


import Prelude hiding((^))
import qualified Prelude


type MathPrimtvId = LaTeX

data MathLaTeX = MathLaTeX
  { exprnKind :: MathExprKind
  , rendrdExpression :: LaTeX
  }

noRedBraces :: LaTeX -> LaTeX
noRedBraces l@(TeXBraces _) = l
noRedBraces l = TeXBraces l



type RendConfReadLaTeX = Reader TeXMathConfiguration LaTeX
type RendConfReadMathLaTeX = Reader TeXMathConfiguration MathLaTeX

data MathLaTeXEval res arg where
  MathEnvd :: Functor list =>
           { mathEnclosingFunc :: list (a -> b) -> c -> d
           , enclosingLaTeX :: list MathLaTeX -> RendConfReadMathLaTeX
           , enclosedMathExpr :: list(MathLaTeXEval b (HCons a c))
           } -> MathLaTeXEval d c


data MathExprKind
  = MathExprAtomSymbol
      -- ^ \"Tight\" symbols, like /x/, but possibly with e.g. subscripts, hat etc..
  | MathExprNumLiteral
  | MathExprCompound
         { fixity :: Fixity
         , heightEstimate :: Maybe BracketSize }
data Fixity = Infix Int   -- ^ For non-associative operators with no generally agreed evaluation direction.
            | InfixA Int  -- ^ For associative operators, i.e. the order doesn't matter and parens can thus be omitted if both operands have the same fixity.
            | Infixl Int  -- ^ For left-associative ops; chaining such from the right will yield parens but not from the left.
            | Infixr Int  -- ^ Right-associative.
            | RightGreedy Int -- ^ Expressions like sums or integrals, which only extend to the right with their given fixity, but never to the left.

-- data MathLaTeXEval res arg
--    = MathLaTeXEval { mathLaTeXevaluation :: MathEvaluation res arg 
--                    , mathLaTeXexprnKind :: MathExprKind
--                    }

onMathCompoundLaTeX :: Fixity -> Maybe BracketSize
                          -> (LaTeX->LaTeX) -> MathLaTeX->MathLaTeX
onMathCompoundLaTeX fxty bSz f (MathLaTeX _ inr)
     = MathLaTeX (MathExprCompound fxty bSz) $ f inr

mathCompound_wFixity :: Fixity -> LaTeX -> RendConfReadMathLaTeX
mathCompound_wFixity fxty = mathCompoundLaTeX fxty Nothing

mathCompoundLaTeX :: Fixity -> Maybe BracketSize -> LaTeX -> RendConfReadMathLaTeX
mathCompoundLaTeX fxty bSz = return . MathLaTeX (MathExprCompound fxty bSz)


-- mathLaTeXexprnFixity :: MathLaTeXEval r a -> Fixity
-- mathLaTeXexprnFixity = xqFixity . mathLaTeXexprnKind
--  where xqFixity MathExprNumLiteral = Infix 9
--        xqFixity MathExprAtomSymbol = Infix 9
--        xqFixity (MathExprCompound f) = f

isotropFixityOf :: MathExprKind -> Int
isotropFixityOf MathExprNumLiteral = 10
isotropFixityOf MathExprAtomSymbol = 10
isotropFixityOf (MathExprCompound fq _)= isotropFixity fq

isotropFixity :: Fixity -> Int
isotropFixity (Infix n) = n
isotropFixity (Infixl n) = n
isotropFixity (Infixr n) = n
isotropFixity (InfixA n) = n
isotropFixity (RightGreedy n) = n
   
autoParensWhenFxtyLT :: Int -> MathLaTeX -> MathLaTeX
autoParensWhenFxtyLT n e@(MathLaTeX knd expr)
 | isotropFixityOf knd < n  = MathLaTeX (MathExprCompound (Infix 9) Nothing) $ autoParens expr
 | otherwise                = e

bracketSizeSuggestion :: MathLaTeX -> Maybe BracketSize
bracketSizeSuggestion (MathLaTeX (MathExprCompound _ bkSz) _) = bkSz
bracketSizeSuggestion _ = Just defaultBracketSize

instance Contravariant (MathLaTeXEval res) where
  contramap f(MathEnvd g wr encld) = MathEnvd g' wr encld'
     where g' l = g l . f
           encld' = fmap(contramap $ \(HCons a c) -> (HCons a $ f c)) encld
-- instance Contravariant (MathLaTeXEval res) where
--   contramap f (MathLaTeXEval e fxty) = MathLaTeXEval (contramap f e) fxty

class PseudoFunctor f where
  pseudoFmap :: (a->b) -> f a c -> f b c

-- 'Contravariant' and 'PseudoFunctor' together form, effectively, 'Flip Arrow'.
-- However, the covariant part isn't really \"correct\" for 'MathLaTeXEval':
-- the result of a mathematical expression can't change while the expression itself
-- remains constant. So we don't export such an instance.
instance PseudoFunctor MathLaTeXEval where
  pseudoFmap φ (MathEnvd ψ l e) = MathEnvd ((φ.).ψ) l e
-- instance PseudoFunctor MathLaTeXEval where
--   pseudoFmap φ (MathLaTeXEval e fxty) = MathLaTeXEval (pseudoFmap φ e) fxty



withArg :: a -> MathLaTeXEval res a -> MathExpr res
withArg = contramap . const

type MathExpr a = MathLaTeXEval a HNil


mathExprRender :: MathLaTeXEval c a -> RendConfReadMathLaTeX
mathExprRender (MathEnvd _ txf enclosed) = do
   cfg <- ask
   txf $ fmap(($cfg) . runReader . mathExprRender ) enclosed 
 
mathExprCalculate :: MathLaTeXEval b arg -> arg -> b
mathExprCalculate = calculated
 where calculated :: MathLaTeXEval d c -> c -> d
       calculated (MathEnvd f _ enclosed) c
            = f (fmap(\e' a
                    -> calculated e' (HCons a c) ) $ enclosed) c

mathExprCalculate_ :: MathExpr b -> b
mathExprCalculate_ x = mathExprCalculate x HNil

mathPrimitiv_ofKind :: MathExprKind -> b -> LaTeX -> MathLaTeXEval b a
mathPrimitiv_ofKind k v name
  = MathEnvd (\None _->v) (\None -> return $ MathLaTeX k name) None

mathPrimitiv, mathNumPrimitiv :: b -> LaTeX -> MathLaTeXEval b a
mathPrimitiv = mathPrimitiv_ofKind MathExprAtomSymbol
mathNumPrimitiv = mathPrimitiv_ofKind MathExprNumLiteral

mathDepPrimitiv :: (a->b) -> LaTeX -> MathLaTeXEval b a
mathDepPrimitiv dv name = MathEnvd (\None->dv) 
                                   (\None -> return $ MathLaTeX MathExprAtomSymbol name)
                                   None

mathVarEntry :: MathPrimtvId -> (a->b) -> MathLaTeXEval b a
mathVarEntry = flip mathDepPrimitiv

polyMathVarEntry :: 
      MathPrimtvId -> (a'->b) -> (forall a. BasedUpon a' a => MathLaTeXEval b a)
polyMathVarEntry name esrc
   = MathEnvd (\None -> esrc . basement) 
              (\None -> return $ MathLaTeX MathExprAtomSymbol name)
              None


mathExprFunction :: (a->r)
                 -> (MathLaTeX -> RendConfReadMathLaTeX)
                 -> MathLaTeXEval a c -> MathLaTeXEval r c
mathExprFunction f fn e = MathEnvd ( \(Identity q) -> f . q )
                                   ( fn . runIdentity )
                                   ( Identity $ contramap hHead e )

mathExprFn :: (a->r) -> MathPrimtvId
                 -> MathLaTeXEval a c -> MathLaTeXEval r c
mathExprFn f fn
   = mathExprFunction f $ mathCompound_wFixity (Infix 9) . funnamer
 where funnamer (MathLaTeX eKind incl)
         | isotropFixityOf eKind <= 9  = fn <> (autoParens incl)
         | otherwise                 = fn <> commS":" <> noRedBraces incl

 
type MathLaTeXInfix = MathLaTeX -> MathLaTeX -> RendConfReadMathLaTeX

mathExprInfix :: (a->a->r)
                 -> MathLaTeXInfix
                 -> MathLaTeXEval a c -> MathLaTeXEval a c -> MathLaTeXEval r c
mathExprInfix ifx ifxn el er
  = MathEnvd ( \(Pair q p) c -> q c `ifx` p c )
             ( \(Pair q p) -> ifxn q p )
             ( Pair (contramap hHead el) (contramap hHead er) )

type Height_InfixPropagation = Maybe BracketSize -> Maybe BracketSize 
                  -> Reader MathHeightsManagement (Maybe BracketSize) 
neglectInfixSelfHeight :: Height_InfixPropagation
neglectInfixSelfHeight _ _ = return Nothing

             
symChoiceIfx :: (a->a->r) -> (MathExprKind -> MathExprKind 
                          -> Reader MathSymbolTranslations LaTeX) 
            -> Fixity -> Height_InfixPropagation
            -> MathLaTeXEval a c -> MathLaTeXEval a c -> MathLaTeXEval r c
symChoiceIfx ifx ifxc fxty bqSzer = mathExprInfix ifx ifxNamer
 where ifxNamer :: MathLaTeXInfix
       ifxNamer lh@(MathLaTeX knL lexpr) rh@(MathLaTeX knR rexpr) = do

          symbsCfg <- askMathSymbolTranslations
          heightCfg <- askMathHeightsManagement
          let rddIfxc = " " <> runReader (ifxc knL knR) symbsCfg 
              bqSzQ = (`runReader`heightCfg) $
                  bqSzer (bracketSizeSuggestion lh) (bracketSizeSuggestion rh)
                               
          mathCompoundLaTeX fxty bqSzQ 
                $ safeLExpr <> " " <> rddIfxc <> " " <> safeRExpr

        where safeLExpr = case(fxty,knL) of
                ( InfixA ε, MathExprCompound lIfx _ )
                   | InfixA κ <- lIfx, ε<=κ  -> lexpr
                   | Infixl κ <- lIfx, ε<=κ  -> lexpr
                ( Infixl ε, MathExprCompound lIfx _ )
                   | InfixA κ <- lIfx, ε<=κ  -> lexpr
                   | Infixl κ <- lIfx, ε<=κ  -> lexpr
                (ε, κ)
                   | isotropFixity ε < isotropFixityOf κ   -> lexpr
                   | otherwise -> autoParens lexpr
              safeRExpr = case(fxty,knR) of
                (_, MathExprCompound (RightGreedy _) _) -> rexpr
                ( InfixA ε, MathExprCompound lIfx _ )
                   | InfixA κ <- lIfx, ε<=κ  -> lexpr
                   | Infixr κ <- lIfx, ε<=κ  -> lexpr
                ( Infixr ε, MathExprCompound lIfx _ )
                   | InfixA κ <- lIfx, ε<=κ  -> lexpr
                   | Infixr κ <- lIfx, ε<=κ  -> lexpr
                (ε, κ)
                   | isotropFixity ε<isotropFixityOf κ   -> rexpr
                   | otherwise -> autoParens rexpr

 
mathExprIfx :: (ea ~ MathLaTeXEval a c, er ~ MathLaTeXEval r c)
      => (a->a->r) -> LaTeX -> Fixity 
          -> ea -> ea -> er
mathExprIfx i s fxty = symChoiceIfx i (\_ _ -> return s) fxty neglectInfixSelfHeight
 

data SubOrSupScrt = Subscript | Superscript

subOrSupScrtIfx :: (a->a->r) -> SubOrSupScrt 
      -> MathLaTeXEval a c -> MathLaTeXEval a c -> MathLaTeXEval r c
subOrSupScrtIfx ifx sscKind = mathExprInfix ifx ifxNamer
 where ifxNamer :: MathLaTeXInfix
       ifxNamer (MathLaTeX knL lexpr) (MathLaTeX knR rexpr)
        = mathCompound_wFixity (Infixr 8) $ mconcat
             [ if isotropFixityOf knL > 8 then lexpr
                                          else autoParens lexpr
             , raw $ case sscKind of Subscript -> "_"
                                     Superscript -> "^"
             , noRedBraces rexpr
             ]

       

mathExpr_hetFn2 :: (a -> b -> r)
                -> (MathLaTeX -> MathLaTeX -> RendConfReadMathLaTeX)
                -> MathLaTeXEval a c -> MathLaTeXEval b c -> MathLaTeXEval r c
mathExpr_hetFn2 ifx ifxn el er
  = MathEnvd ( \(Pair q p) c -> fst(q c) `ifx` snd(p c) )
             ( \(Pair q p) -> ifxn q p )
             ( Pair ( pseudoFmap coFst $ contramap hHead el )
                    ( pseudoFmap coSnd $ contramap hHead er ) )


                                   
autoLaTeXBrackets :: String -> String -> MathLaTeX -> RendConfReadMathLaTeX
autoLaTeXBrackets lBr rBr (MathLaTeX nKnd inr)
   = mathCompound_wFixity (Infix 9) $ case nKnd of
       MathExprCompound _ bSz  -> latexBrackets bSz lBr rBr inr
       _                       -> latexBrackets (Just 0) lBr rBr inr
                                   

instance (Num res) => Num (MathLaTeXEval res arg) where
  fromInteger n = mathNumPrimitiv (fromInteger n) (rendertex n)
  
  (+) = mathExprIfx (+) "+" $ InfixA 6
  (-) = mathExprIfx (-) "-" $ Infixl 6
  negate = mathExprFunction negate $
               \(MathLaTeX nKnd inr) -> mathCompound_wFixity (Infixl 6) $
                 "-"<> (if isotropFixityOf nKnd <= 6 then autoParens else id)
                       (noRedBraces inr)
         
  (*) = autoMult
  
  signum = mathExprFn abs (mathrm"sgn")
  abs = mathExprFunction abs $ autoLaTeXBrackets "|" "|"


autoMult, defaultMult, atomVarMult, numLiteralMult
  :: (Num res, e ~ MathLaTeXEval res arg) => e -> e -> e
defaultMult    = symChoiceIfx (*) (\_ _ -> reader defMultiplicationSymbol    ) (InfixA 7) neglectInfixSelfHeight
numLiteralMult = symChoiceIfx (*) (\_ _ -> reader numeralMultiplicationSymbol) (InfixA 7) neglectInfixSelfHeight
atomVarMult    = symChoiceIfx (*) (\_ _ -> reader atomVarMultiplicationSymbol) (InfixA 7) neglectInfixSelfHeight
autoMult       = symChoiceIfx (*) ((reader.) . acs) (InfixA 7) neglectInfixSelfHeight
 where acs MathExprAtomSymbol MathExprAtomSymbol = atomVarMultiplicationSymbol
       acs MathExprNumLiteral MathExprAtomSymbol = atomVarMultiplicationSymbol
       acs MathExprNumLiteral _                  = numeralMultiplicationSymbol
       acs _                  MathExprNumLiteral = numeralMultiplicationSymbol
       acs _                  _                  = defMultiplicationSymbol
 



infixr 8 ^
class Num x => Powerable x where
  (^) :: x -> x -> x
instance Powerable Int where { (^) = (Prelude.^) }
instance Powerable Double where { (^) = (**) }
instance Powerable Float where { (^) = (**) }
instance Powerable Integer where { (^) = (Prelude.^) }
instance (Powerable res, Show res) => Powerable (MathLaTeXEval res arg) where
  (^) = subOrSupScrtIfx (^) Superscript


instance (Fractional res) => Fractional (MathLaTeXEval res arg) where
--   fromRational e = mathExprInfix (/)
--      (`mathCompound_wFixity`Infix 9) $ mathExprInfix (/)
--            (\n d -> return . TeXComm "tfrac" $ map FixArg [n,d] )
--    where [n,d] = map(fromIntegral . ($e)) [numerator, denominator]
  fromRational e = fromInteger(numerator e) / fromInteger(denominator e)
  
  a/b = mathExprInfix (/) 
          (\n d -> mathCompound_wFixity (Infix 8) -- Fixity like (^), but not right-associative.
                     . TeXComm(fracChoice (exprnKind n) (exprnKind d))
                     $ map (FixArg . rendrdExpression) [n,d] ) a b
   where fracChoice (MathExprCompound _ _) _ = "frac"
         fracChoice _ (MathExprCompound _ _) = "frac"
         fracChoice _           _          = "tfrac"
  
  recip = mathExprFunction recip (uncurry mathCompound_wFixity . rcper)
   where rcper (MathLaTeX (MathExprCompound q _) inr)
          | isotropFixity q>8 = (Infixr 8, autoParens inr ^: braces"-1")
         rcper (MathLaTeX MathExprNumLiteral inr)
            = (Infix 8, TeXComm "frac1" . (:[]) $ FixArg inr)
         rcper (MathLaTeX _ inr) = (Infixr 8, noRedBraces inr ^: braces"-1")

instance (Floating res) => Floating (MathLaTeXEval res arg) where
  pi = mathPrimitiv pi pi_
  
  sqrt = mathExprFunction sqrt $
              return . onMathCompoundLaTeX (Infix 10) Nothing
                            (TeXComm "sqrt" .(:[]). FixArg)
           
  exp = (mathPrimitiv (exp 1) "e" **)
--    (`mathCompound_wFixity`Infix 8) mathExprFunction exp (return . ("e"^:))
           
  (**) = subOrSupScrtIfx (**) Superscript
           
  log = mathExprFn log ln
  logBase = mathExprInfix logBase lbR
   where lbR β τ = case exprnKind β of
           MathExprCompound _ _
             -> mathCompound_wFixity(Infix 8) . TeXComm "frac"
                     $ map (FixArg . (ln<>) . rendrdExpression 
                                         . autoParensWhenFxtyLT 9) [β, τ]
           _ -> mathCompound_wFixity(Infix 9) $ tlog !: noRedBraces(rendrdExpression β)
                                                  <> autoParens (rendrdExpression τ)
  
  sin = mathExprFn sin tsin
  cos = mathExprFn cos tcos
  tan = mathExprFn tan ttan
  asin = mathExprFn asin arcsin
  acos = mathExprFn acos arccos
  atan = mathExprFn atan arctan
  sinh = mathExprFn sinh tsinh
  cosh = mathExprFn cosh tcosh
  tanh = mathExprFn tanh ttanh
  asinh = mathExprFn asinh $ mathrm "arcsinh"
  acosh = mathExprFn acosh $ mathrm "arccosh"
  atanh = mathExprFn atanh $ mathrm "arctanh"
  


instance (ComplexC r, RealFloat(RealAxis r))
             => ComplexC(MathLaTeXEval r arg) where
  type RealAxis(MathLaTeXEval r arg) = MathLaTeXEval (RealAxis r) arg
  
  imagUnit = mathPrimitiv imagUnit "i"
  
  realAsComplex = pseudoFmap realAsComplex
  imagAsComplex = (imagUnit *) . pseudoFmap realAsComplex
  
  conjugate = mathExprFunction conjugate $
               return . onMathCompoundLaTeX (Infix 10) Nothing
                          (TeXComm "overline" . (:[]) . FixArg . noRedBraces)
  realPart = mathExprFn realPart $ TeXCommS "Re"
  imagPart = mathExprFn imagPart $ TeXCommS "Im"

  magnitude = mathExprFunction magnitude $ autoLaTeXBrackets "|" "|"
  phase     = mathExprFn phase $ TeXCommS "arg"
 
-- instance (Enum r, Show r) => Enum (MathExpr


-- | Gather, for values taken from some set (represented by a list type),
-- the output of some function.
lSetFold_bigSymb :: forall rng res a sumVarDep svdStack .
              ( Monoid res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a )
       => Fixity -> MathPrimtvId -> LaTeX
        -> MathLaTeXEval [rng] a
                 -> ( MathLaTeXEval rng svdStack
                     -> MathLaTeXEval res sumVarDep )
                 -> MathLaTeXEval res a
lSetFold_bigSymb fxty sumVar folderVis rngSpec summand
   = MathEnvd (                                    (
                \(Pair rngG summandG) _ -> 
                    mconcat [ fst $ summandG x
                     | x<-snd $ rngG undefined ]
                                         ) :: Pair(rng -> (res, [rng])) -> a -> res )
              ( \(Pair rngV summandV) -> mathCompound_wFixity fxty $
                     (folderVis !: noRedBraces(sumVar `in_` rendrdExpression
                                                         (autoParensWhenFxtyLT 4 rngV )) )
                                      <> rendrdExpression (autoParensWhenFxtyLT 
                                                               (isotropFixity fxty) summandV)
                          )
              ( Pair ( pseudoFmap coSnd 
                        $ contramap hTail rngSpec )
                     ( pseudoFmap coFst
                           . summand $ mathVarEntry sumVar
                                                    (hHead.(basement :: svdStack->sumVarDep)) )
                                              :: Pair(MathLaTeXEval (res, [rng]) (HCons rng a) ) )

polyLSetFold_bigSymb :: forall rng res a sumVarDep .
              ( Monoid res
              , sumVarDep ~ HCons rng a )
       => Fixity -> MathPrimtvId -> LaTeX
        -> MathLaTeXEval [rng] a
                 -> ( ( forall svdStack . BasedUpon sumVarDep svdStack
                         => MathLaTeXEval rng svdStack                 )
                     -> MathLaTeXEval res sumVarDep )
                 -> MathLaTeXEval res a
polyLSetFold_bigSymb fxty sumVar folderVis rngSpec summand
    = MathEnvd (                                    (
                 \(Pair rngG summandG) _ -> 
                     mconcat [ fst $ summandG x
                      | x<-snd $ rngG undefined ]
                                          ) :: Pair(rng -> (res, [rng])) -> a -> res )
               ( \(Pair rngV summandV) -> mathCompound_wFixity fxty $
                      (folderVis !: noRedBraces(sumVar `in_` rendrdExpression
                                                     (autoParensWhenFxtyLT 4 rngV )) )
                                  <> rendrdExpression (autoParensWhenFxtyLT 
                                                          (isotropFixity fxty) summandV)
                           )
               ( Pair ( pseudoFmap coSnd 
                         $ contramap hTail rngSpec )
                      ( pseudoFmap coFst
                            $ summand 
                              ( polyMathVarEntry sumVar 
                                  ( hHead :: sumVarDep -> rng )
                                :: forall svdStack' . BasedUpon sumVarDep svdStack'
                                                     => MathLaTeXEval rng svdStack'
                                                    ) )
                                          :: Pair(MathLaTeXEval (res, [rng]) (HCons rng a) ) )

-- | A list only represents a set properly when there are no duplicate elements,
-- a precondition which this function doesn't (and can't!) check.
listAsFinSet :: [MathLaTeXEval r a] -> MathLaTeXEval [r] a
listAsFinSet ls
 = MathEnvd ( const . map($HNil) )
            ( mathCompound_wFixity(Infix 9) . autoBraces 
                  . mconcat . intersperse(raw",") . map rendrdExpression )
            ( map (contramap hTail) ls )


-- | Gather the results over some range. The usual @ᵢ₌₁Σ³ aᵢ⋅bᵢ@-thing.
limsFold_bigSymb :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Monoid res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a ) =>
      Fixity -> MathPrimtvId -> LaTeX
        -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( MathLaTeXEval rng svdStack
              -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
limsFold_bigSymb fxty sumVar folderVis lBound uBound summand
 = MathEnvd ( \(Triple rngLG rngUG summandG) _ ->
                 mconcat [ fst $ summandG x
                  | x<-[snd $ rngLG undefined .. snd $ rngUG undefined] ] ) 
            ( \(Triple rngLV rngUV summandV) -> mathCompound_wFixity fxty $
                  (folderVis !: noRedBraces(sumVar =: rendrdExpression
                                                   (autoParensWhenFxtyLT 4 rngLV))
                                  ^: noRedBraces(rendrdExpression rngUV)        ) 
                       <> rendrdExpression (autoParensWhenFxtyLT (isotropFixity fxty) summandV ) )
            ( Triple (pseudoFmap coSnd $ contramap hTail lBound )
                     (pseudoFmap coSnd $ contramap hTail uBound )
                     (pseudoFmap coFst . summand
                        $ mathVarEntry sumVar (hHead.(basement :: svdStack->sumVarDep))                     ) )
 
-- | Just as 'limsFold_bigSymb', but as a Rank3-function. This allows the summation-variable to
-- be used in multiple different closures, i.e. in different nesting-depths of local-sums
-- (recall that variables are type-parameterised on the entire closure).
-- However, rank>1-polymorphism cannot in general be type-infered, so you may need
-- to provide explicit signatures, which will tend to be less than beautiful. Often,
-- the simpler 'limsFold_bigSymb' will also work and should be preferred.
polyLimsFold_bigSymb :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Monoid res
              , sumVarDep ~ HCons rng a ) =>
      Fixity -> MathPrimtvId -> LaTeX
        -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( (forall svdStack. BasedUpon sumVarDep svdStack
                 => MathLaTeXEval rng svdStack) -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
polyLimsFold_bigSymb fxty sumVar folderVis lBound uBound summand
 = MathEnvd ( \(Triple rngLG rngUG summandG) _ ->
                 mconcat [ fst $ summandG x
                  | x<-[snd $ rngLG undefined .. snd $ rngUG undefined] ] ) 
            ( \(Triple rngLV rngUV summandV) -> mathCompound_wFixity fxty $
                  (folderVis !: noRedBraces(sumVar =: rendrdExpression
                                                       (autoParensWhenFxtyLT 4 rngLV))
                                  ^: noRedBraces(rendrdExpression rngUV)        ) 
                       <> rendrdExpression (autoParensWhenFxtyLT (isotropFixity fxty) summandV ) )
            ( Triple (pseudoFmap coSnd $ contramap hTail lBound )
                     (pseudoFmap coSnd $ contramap hTail uBound )
                     (pseudoFmap coFst $ summand
                          ( polyMathVarEntry sumVar 
                                             ( hHead :: sumVarDep -> rng )
                         :: forall svdStack' . BasedUpon sumVarDep svdStack'
                                       => MathLaTeXEval rng svdStack'
                        ) ) )
           

-- | Like all the specific sum and product functions, 'lSetSum' is merely
-- the obvious instantiation of its specific generic fold correspondent,
-- in this case 'lSetFold_bigSymb'.
lSetSum, lSetProd :: forall rng res a sumVarDep svdStack .
              ( Num res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a )
       => MathPrimtvId -> MathLaTeXEval [rng] a
                 -> ( MathLaTeXEval rng svdStack
                     -> MathLaTeXEval res sumVarDep )
                 -> MathLaTeXEval res a
lSetSum sv rngLG = pseudoFmap getSum
                    . lSetFold_bigSymb (RightGreedy 6) sv (TeXCommS "sum") rngLG
                    . (pseudoFmap Sum .)
lSetProd sv rngLG = pseudoFmap getProduct
                    . lSetFold_bigSymb (RightGreedy 7) sv (TeXCommS "prod") rngLG
                    . (pseudoFmap Product .)
 
polyLSetSum, polyLSetProd :: forall rng res a sumVarDep .
              ( Num res
              , sumVarDep ~ HCons rng a )
       => MathPrimtvId -> MathLaTeXEval [rng] a
                 -> ( ( forall svdStack . BasedUpon sumVarDep svdStack
                         => MathLaTeXEval rng svdStack                 )
                     -> MathLaTeXEval res sumVarDep )
                 -> MathLaTeXEval res a
polyLSetSum sv rngLG = pseudoFmap getSum
                    . polyLSetFold_bigSymb (RightGreedy 6) sv (TeXCommS "sum") rngLG
                    . (pseudoFmap Sum .)
polyLSetProd sv rngLG = pseudoFmap getProduct
                    . polyLSetFold_bigSymb (RightGreedy 7) sv (TeXCommS "prod") rngLG
                    . (pseudoFmap Product .)
 
limsSum, limsProd :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Num res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( MathLaTeXEval rng svdStack
              -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
limsSum sv lBG uBG = pseudoFmap getSum
                    . limsFold_bigSymb (RightGreedy 6) sv (TeXCommS "sum") lBG uBG
                    . (pseudoFmap Sum .)
limsProd sv lBG uBG = pseudoFmap getProduct
                    . limsFold_bigSymb (RightGreedy 7) sv (TeXCommS "prod") lBG uBG
                    . (pseudoFmap Product .)

polyLimsSum, polyLimsProd :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Num res
              , sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( (forall svdStack. BasedUpon sumVarDep svdStack
                 => MathLaTeXEval rng svdStack) -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
polyLimsSum sv lBG uBG = pseudoFmap getSum
                    . polyLimsFold_bigSymb (RightGreedy 6) sv (TeXCommS "sum") lBG uBG
                    . (pseudoFmap Sum .)
polyLimsProd sv lBG uBG = pseudoFmap getProduct
                    . polyLimsFold_bigSymb (RightGreedy 7) sv (TeXCommS "prod") lBG uBG
                    . (pseudoFmap Product .)


coFst :: a -> (a,b)
coSnd :: b -> (a,b)
coFst = (,undefined)
coSnd = (undefined,)
