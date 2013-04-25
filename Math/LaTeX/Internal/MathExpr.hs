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
 
            

data MathEvaluation res arg where
  MathEnvd :: Functor list =>
           { mathEnclosingFunc :: list (a -> b) -> c -> d
           , enclosingLaTeX :: list LaTeX -> LaTeX
           , enclosedMathExpr :: list(MathLaTeXEval b (HCons a c))
           } -> MathEvaluation d c

data MathExprKind
  = MathExprNumLiteral
  | MathExprAtomVariable
  | MathExprCompound Fixity
data Fixity = Infix Int
            | Infixl Int
            | Infixr Int
            | RightGreedy Int

data MathLaTeXEval res arg
   = MathLaTeXEval { mathLaTeXevaluation :: MathEvaluation res arg 
                   , mathLaTeXexprnKind :: MathExprKind
                   }

mathCompound_wFixity :: MathEvaluation r a -> Fixity -> MathLaTeXEval r a
mathCompound_wFixity e = MathLaTeXEval e . MathExprCompound

mathLaTeXexprnFixity :: MathLaTeXEval r a -> Fixity
mathLaTeXexprnFixity = xqFixity . mathLaTeXexprnKind
 where xqFixity MathExprNumLiteral = Infix 9
       xqFixity MathExprAtomVariable = Infix 9
       xqFixity (MathExprCompound f) = f

isotropFixity :: MathExprKind -> Int
isotropFixity = isotropise . mathLaTeXexprnFixity . MathLaTeXEval undefined
 where isotropise (Infix n) = n
       isotropise (Infixl n) = n
       isotropise (Infixr n) = n
       isotropise (RightGreedy n) = n

instance Contravariant (MathEvaluation res) where
  contramap f(MathEnvd g wr encld) = MathEnvd g' wr encld'
     where g' l = g l . f
           encld' = fmap(contramap $ \(HCons a c) -> (HCons a $ f c)) encld
instance Contravariant (MathLaTeXEval res) where
  contramap f (MathLaTeXEval e fxty) = MathLaTeXEval (contramap f e) fxty

class PseudoFunctor f where
  pseudoFmap :: (a->b) -> f a c -> f b c

-- 'Contravariant' and 'PseudoFunctor' together form, effectively, 'Flip Arrow'.
-- However, the covariant part isn't really \"correct\" for 'MathLaTeXEval':
-- the result of a mathematical expression can't change while the expression itself
-- remains constant. So we don't export such an instance.
instance PseudoFunctor MathEvaluation where
  pseudoFmap φ (MathEnvd ψ l e) = MathEnvd ((φ.).ψ) l e
instance PseudoFunctor MathLaTeXEval where
  pseudoFmap φ (MathLaTeXEval e fxty) = MathLaTeXEval (pseudoFmap φ e) fxty



withArg :: a -> MathLaTeXEval res a -> MathExpr res
withArg = contramap . const

type MathExpr a = MathLaTeXEval a HNil


mathExprRender :: MathLaTeXEval b arg -> LaTeX
mathExprRender (MathLaTeXEval e _) = rendered e
 where rendered :: MathEvaluation c a -> LaTeX
       rendered (MathEnvd _ txf enclosed)
            = txf . fmap(rendered . mathLaTeXevaluation) $ enclosed
 
mathExprCalculate :: MathLaTeXEval b arg -> arg -> b
mathExprCalculate (MathLaTeXEval e _) = calculated e
 where calculated :: MathEvaluation d c -> c -> d
       calculated (MathEnvd f _ enclosed) c
            = f (fmap(\(MathLaTeXEval e' _) a
                    -> calculated e' (HCons a c) ) $ enclosed) c

mathExprCalculate_ :: MathExpr b -> b
mathExprCalculate_ x = mathExprCalculate x HNil

mathPrimitiv :: b -> LaTeX -> MathLaTeXEval b a
-- mathPrimitiv v name = MathLaTeXEval (MathPrimitive v name) 10
mathPrimitiv v name
  = (MathEnvd (\None _->v) (\None -> name) None) `mathCompound_wFixity` Infix 10

mathDepPrimitiv :: (a->b) -> LaTeX -> MathLaTeXEval b a
mathDepPrimitiv dv name
  = (MathEnvd (\None->dv) (\None -> name) None) `mathCompound_wFixity` Infix 10

mathVarEntry :: MathPrimtvId -> (a->b) -> MathLaTeXEval b a
mathVarEntry name esrc
   = MathEnvd (\None -> esrc) (\None -> name) None `mathCompound_wFixity` Infix 10

polyMathVarEntry :: 
      MathPrimtvId -> (a'->b) -> (forall a. BasedUpon a' a => MathLaTeXEval b a)
polyMathVarEntry name esrc
   = MathEnvd (\None -> esrc . basement) (\None -> name) None `mathCompound_wFixity` Infix 10


mathExprFunction :: (a->r)
                 -> (MathPrimtvId -> MathPrimtvId)
                 -> MathLaTeXEval a c -> MathEvaluation r c
mathExprFunction f fn e = MathEnvd ( \(Identity q) -> f . q )
                                   ( fn . runIdentity )
                                   ( Identity $ contramap hHead e )
   
mathExprFn :: (a->r) -> MathPrimtvId
                 -> MathLaTeXEval a c -> MathLaTeXEval r c
mathExprFn f fn e@(MathLaTeXEval _ fxty)
   = mathCompound_wFixity (mathExprFunction f funnamer e) $ Infix 9
 where funnamer incl
         | isotropFixity fxty <= 9   = fn <> braces (autoParens incl)
         | otherwise                 = fn <> commS":" <> braces incl

 

mathExprInfix :: (a->a->r)
                 -> (LaTeX -> LaTeX -> LaTeX)
                 -> MathLaTeXEval a c -> MathLaTeXEval a c -> MathEvaluation r c
mathExprInfix ifx ifxn el er
  = MathEnvd ( \(Pair q p) c -> q c `ifx` p c )
             ( \(Pair q p) -> ifxn q p )
             ( Pair (contramap hHead el) (contramap hHead er) )
             
mathExprIfx :: (a->a->r) -> MathPrimtvId -> Fixity
                 -> MathLaTeXEval a c -> MathLaTeXEval a c ->  MathLaTeXEval r c
mathExprIfx ifx ifxn fxty el@(MathLaTeXEval _ fxtl) er@(MathLaTeXEval _ fxtr)
    = MathLaTeXEval (mathExprInfix ifx ifxNamer el er) $ MathExprCompound fxty
 where ifxNamer lexpr rexpr = mconcat
                  [ case(fxty,fxtl) of
                      ( Infixl ε, MathExprCompound (Infixl κ) )
                         | ε<=κ  -> plain lexpr
                      (ε, κ)
                         | isotropFixity (MathExprCompound ε)
                             < isotropFixity κ   -> plain lexpr
                         | otherwise -> parenthd lexpr
                  , " ", ifxn, " "
                  , case(fxty,fxtr) of
                      (_, MathExprCompound (RightGreedy _)) -> plain rexpr
                      (Infixr ε, MathExprCompound (Infixr κ) )
                         | ε<=κ  -> plain rexpr
                      (ε, κ)
                         | isotropFixity (MathExprCompound ε)<isotropFixity κ   -> plain rexpr
                         | otherwise -> parenthd rexpr
                  ]
       plain expr = braces expr
       parenthd = braces . autoParens
       

mathExpr_hetFn2 :: (a -> b -> r)
                -> (LaTeX -> LaTeX -> LaTeX)
                -> MathLaTeXEval a c -> MathLaTeXEval b c -> MathEvaluation r c
mathExpr_hetFn2 ifx ifxn el er
  = MathEnvd ( \(Pair q p) c -> fst(q c) `ifx` snd(p c) )
             ( \(Pair q p) -> ifxn q p )
             ( Pair ( pseudoFmap coFst $ contramap hHead el )
                    ( pseudoFmap coSnd $ contramap hHead er ) )


-- mathExprInfix :: (a->b->r) -> MathPrimtvId -> Fixity
--     -> MathLaTeXEval c a -> MathLaTeXEval c b -> MathLaTeXEval c r
-- mathExprInfix ifx ifxn fxty (MathLaTeXEval a fxta) (MathLaTeXEval b fxtb)
--    = MathLaTeXEval resExprn fxty 
--  where resexprn
--          = MathEnvd ( \q c -> q $ c c )
--                     ( "" )
--                     ( \inc -> `T.concat`[ fn, "\\left(", inc, "\\right" ] )
--        ifxRender
--         | fxty < min fxta fxtb  = 
--         | otherwise             = 
                                   

instance (Num res) => Num (MathLaTeXEval res arg) where
  fromInteger n = mathPrimitiv (fromInteger n) (rendertex n)
  
  (+) = mathExprIfx (+) "+" $ Infixl 6
  (-) = mathExprIfx (-) "-" $ Infixl 6
  negate q@(MathLaTeXEval _ fxty) = case fxty of
      (MathExprCompound (RightGreedy n)) 
                              -> mathCompound_wFixity res . Infixl $ min 6 n
      y | isotropFixity y > 6 -> res `mathCompound_wFixity` Infixl 6
        | otherwise           -> mathExprFunction negate
                                   (\inr -> "-"<>autoParens(braces inr)<>"") q
                                  `mathCompound_wFixity` Infixl 6
   where res =  mathExprFunction negate (("-"<>).braces) q
  (*) = mathExprIfx (*) (commS"cdot") $ Infixl 7
  
  signum = mathExprFn abs (mathrm"sgn")
  abs = (`mathCompound_wFixity`Infix 9) . mathExprFunction abs
           (autoBrackets "|" "|")

 
 

infixr 8 ^
class Num x => Powerable x where
  (^) :: x -> x -> x
instance Powerable Int where { (^) = (Prelude.^) }
instance Powerable Double where { (^) = (**) }
instance Powerable Float where { (^) = (**) }
instance Powerable Integer where { (^) = (Prelude.^) }
instance (Powerable res, Show res) => Powerable (MathLaTeXEval res arg) where
  (^) = mathExprIfx (^) (raw"^") $ Infixr 8


instance (Fractional res) => Fractional (MathLaTeXEval res arg) where
  fromRational e = (`mathCompound_wFixity`Infix 9) $ mathExprInfix (/)
           (\n d -> TeXComm "tfrac" $ map FixArg [n,d] )
           (fromIntegral $ numerator e) (fromIntegral $ denominator e)
  
  a/b = (`mathCompound_wFixity`Infix 9) $ mathExprInfix (/)
           (\n d -> TeXComm "frac" $ map FixArg [n,d] ) a b
  
  recip = (`mathCompound_wFixity`Infix 9) . mathExprFunction recip
           (TeXComm "frac1" . (:[]) . FixArg)

instance (Floating res) => Floating (MathLaTeXEval res arg) where
  pi = mathPrimitiv pi pi_
  
  sqrt = (`mathCompound_wFixity`Infix 9) . mathExprFunction sqrt
              (TeXComm "sqrt" .(:[]). FixArg)
           
  exp = (`mathCompound_wFixity`Infix 8) . mathExprFunction exp ("e" ^:)
--   b**x = (`mathCompound_wFixity`Infixr 8) $ mathExprInfix (**)
  (**) = mathExprIfx (**) (raw"^") $ Infixr 8
--            (\β ξ -> T.concat [ "{", β, "}^{", ξ, "}"] ) b x
           
  log = mathExprFn log ln
  logBase b t = (`mathCompound_wFixity`Infix 9) $ mathExprInfix logBase
           (\β τ -> tlog !: β <> autoParens τ ) b t
  
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
  
  conjugate = (`mathCompound_wFixity` Infix 10) .
                  mathExprFunction conjugate (TeXComm "overline" . (:[]) . FixArg . braces)
  realPart = mathExprFn realPart $ TeXCommS "Re"
  imagPart = mathExprFn imagPart $ TeXCommS "Im"

  magnitude = (`mathCompound_wFixity`Infix 9) . mathExprFunction magnitude
           (autoBrackets "|" "|")
  phase     = mathExprFn phase $ TeXCommS "arg"
 
-- instance (Enum r, Show r) => Enum (MathExpr


-- | Gather, for values taken from some set (represented by a list type),
-- the output of some function.
lSetFold_bigSymb :: forall rng res a sumVarDep svdStack .
              ( Monoid res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a )
       => MathPrimtvId -> LaTeX
        -> MathLaTeXEval [rng] a
                 -> ( MathLaTeXEval rng svdStack
                     -> MathLaTeXEval res sumVarDep )
                 -> MathLaTeXEval res a
lSetFold_bigSymb sumVar folderVis rngSpec summand = sumExpr `mathCompound_wFixity` RightGreedy 6
 where sumExpr 
        = MathEnvd (                                    (
                     \(Pair rngG summandG) _ -> 
                         mconcat [ fst $ summandG x
                          | x<-snd $ rngG undefined ]
                                              ) :: Pair(rng -> (res, [rng])) -> a -> res )
                   ( \(Pair rngV summandV) -> 
                          (folderVis !: braces(sumVar `in_` rngV)) <> summandV 
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
       => MathPrimtvId -> LaTeX
        -> MathLaTeXEval [rng] a
                 -> ( ( forall svdStack . BasedUpon sumVarDep svdStack
                         => MathLaTeXEval rng svdStack                 )
                     -> MathLaTeXEval res sumVarDep )
                 -> MathLaTeXEval res a
polyLSetFold_bigSymb sumVar folderVis rngSpec summand = sumExpr `mathCompound_wFixity` RightGreedy 6
 where sumExpr 
        = MathEnvd (                                    (
                     \(Pair rngG summandG) _ -> 
                         mconcat [ fst $ summandG x
                          | x<-snd $ rngG undefined ]
                                              ) :: Pair(rng -> (res, [rng])) -> a -> res )
                   ( \(Pair rngV summandV) -> 
                          (folderVis !: braces(sumVar `in_` rngV)) <> summandV 
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
listAsFinSet ls = listExpr `mathCompound_wFixity` Infix 9
 where listExpr = MathEnvd ( const . map($HNil) )
                           ( autoBraces . mconcat . intersperse(raw",") )
                           ( map (contramap hTail) ls )


-- | Gather the results over some range. The usual @ᵢ₌₁Σ³ aᵢ⋅bᵢ@-thing.
limsFold_bigSymb :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Monoid res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> LaTeX
        -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( MathLaTeXEval rng svdStack
              -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
limsFold_bigSymb sumVar folderVis lBound uBound summand
  = sumExpr `mathCompound_wFixity` RightGreedy 6
 where sumExpr = MathEnvd ( \(Triple rngLG rngUG summandG) _ ->
                               mconcat [ fst $ summandG x
                                | x<-[snd $ rngLG undefined .. snd $ rngUG undefined] ] ) 
                          ( \(Triple rngLV rngUV summandV) ->
                                (folderVis !: braces(sumVar =: rngLV)
                                                ^: braces(rngUV)        ) <> summandV )
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
      MathPrimtvId -> LaTeX
        -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( (forall svdStack. BasedUpon sumVarDep svdStack
                 => MathLaTeXEval rng svdStack) -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
polyLimsFold_bigSymb sumVar folderVis lBound uBound summand
  = sumExpr `mathCompound_wFixity` RightGreedy 6
 where sumExpr = MathEnvd ( \(Triple rngLG rngUG summandG) _ ->
                               mconcat [ fst $ summandG x
                                | x<-[snd $ rngLG undefined .. snd $ rngUG undefined] ] ) 
                          ( \(Triple rngLV rngUV summandV) ->
                                (folderVis !: braces(sumVar =: rngLV)
                                                ^: braces(rngUV)        ) <> summandV )
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
                    . lSetFold_bigSymb sv (TeXCommS "sum") rngLG
                    . (pseudoFmap Sum .)
lSetProd sv rngLG = pseudoFmap getProduct
                    . lSetFold_bigSymb sv (TeXCommS "prod") rngLG
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
                    . polyLSetFold_bigSymb sv (TeXCommS "sum") rngLG
                    . (pseudoFmap Sum .)
polyLSetProd sv rngLG = pseudoFmap getProduct
                    . polyLSetFold_bigSymb sv (TeXCommS "prod") rngLG
                    . (pseudoFmap Product .)
 
limsSum, limsProd :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Num res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( MathLaTeXEval rng svdStack
              -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
limsSum sv lBG uBG = pseudoFmap getSum
                    . limsFold_bigSymb sv (TeXCommS "sum") lBG uBG
                    . (pseudoFmap Sum .)
limsProd sv lBG uBG = pseudoFmap getProduct
                    . limsFold_bigSymb sv (TeXCommS "prod") lBG uBG
                    . (pseudoFmap Product .)

polyLimsSum, polyLimsProd :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Num res
              , sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( (forall svdStack. BasedUpon sumVarDep svdStack
                 => MathLaTeXEval rng svdStack) -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
polyLimsSum sv lBG uBG = pseudoFmap getSum
                    . polyLimsFold_bigSymb sv (TeXCommS "sum") lBG uBG
                    . (pseudoFmap Sum .)
polyLimsProd sv lBG uBG = pseudoFmap getProduct
                    . polyLimsFold_bigSymb sv (TeXCommS "prod") lBG uBG
                    . (pseudoFmap Product .)


coFst :: a -> (a,b)
coSnd :: b -> (a,b)
coFst = (,undefined)
coSnd = (undefined,)
