-- |
-- Module      : Math.LaTeX.Prelude
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
-- {-# LANGUAGE OverlappingInstances             #-}
{-# LANGUAGE PatternGuards                    #-}
{-# LANGUAGE TypeFamilies                     #-}
{-# LANGUAGE RankNTypes                       #-}
{-# LANGUAGE TupleSections                    #-}
{-# LANGUAGE RecordWildCards                  #-}

module Math.LaTeX.Prelude (
    -- * Data types
    MathPrimtvId
  , MathLaTeXEval
  , MathExpr
  , ComparisonsEval
    -- * Arithmetic calculations
    -- ** Sums
  , finRSum , polyFinRSum
  , lSetSum , polyLSetSum
    -- ** Products
  , finRProd , polyFinRProd
  , lSetProd , polyLSetProd
    -- ** Generic folds
  , finRFold_bigSymb , polyFinRFold_bigSymb
  , lSetFold_bigSymb , polyLSetFold_bigSymb
  , listAsFinSet
    -- * Rendering
  , mathExprRender
  , mathExprCalculate , mathExprCalculate_
  , inlineMathExpr , inlineMathExpr_
  , inlineMathShow , inlineRoughValue
  , (?~?), (?=?), (...:)
  , displayMathExpr , displayMathExpr_
  , displayMathExpr_wRResult
  , displayMathCompareSeq , displayMathCompareSeq_
  , mathExprEvalRough
    -- * Construction
  , mathPrimitiv , mathDepPrimitiv
  , mathExprFunction, mathExprFn
  , mathExprInfix, mathExprIfx
  , mathDefinition
  , prettyFloatApprox
    -- * Equivalency-relation classes
  , Equatable(..)
  , Orderable(..)
  , RoughEqable(..)
--                           , MagnitudeOrd(..)
  , Powerable(..)
    -- * The rendering monad
  , MathematicalLaTeX, MathematicalLaTeX_
  , MathematicalLaTeXT, MathematicalLaTeXT_
  , wDefaultConf_toHaTeX
  , fromHaTeX
  , nl
  ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Data.Foldable(fold)
import Data.Bifoldable
import Data.Function
import Data.Functor.Contravariant
import Data.Bifunctor

import Data.List
import Data.HList
import Data.HList.BasedUpon
import Data.Chain
import Data.Functor.FixedLength

import Data.Ratio
import Data.Complex(Complex(..))
import Data.Complex.Class
import Data.String







type MathPrimtvId = LaTeX
data Fixity = Infix Int
            | Infixl Int
            | Infixr Int
            | RightGreedy Int
isotropFixity :: Fixity -> Int
isotropFixity (Infix n) = n
isotropFixity (Infixl n) = n
isotropFixity (Infixr n) = n
isotropFixity (RightGreedy n) = n

data MathEvaluation res arg where
--   MathPrimitive { mathPrimVal :: res              -- MathEnvd is actually a generalisation 
--                 , mathPrimId :: MathPrimtvId      -- of all possible contructors; it's not
--                 } :: MathEvaluation arg res       -- particularly efficient though.
--   MathDepdPrimitive { mathDpPrimtveVal :: arg -> res
--                     , mathDpPrimtveId :: MathPrimtvId -> MathPrimtvId
--                     } :: MathEvaluation arg res
  MathEnvd :: Functor list =>
           { mathEnclosingFunc :: list (a -> b) -> c -> d
           , enclosingLaTeX :: list LaTeX -> LaTeX
           , enclosedMathExpr :: list(MathLaTeXEval b (HCons a c))
           } -> MathEvaluation d c

data MathLaTeXEval res arg
      = MathLaTeXEval { mathLaTeXevaluation :: MathEvaluation res arg 
                      , mathLaTeXexprnFixity :: Fixity
                      }

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
  = (MathEnvd (\None _->v) (\None -> name) None) `MathLaTeXEval` Infix 10

mathDepPrimitiv :: (a->b) -> LaTeX -> MathLaTeXEval b a
mathDepPrimitiv dv name
  = (MathEnvd (\None->dv) (\None -> name) None) `MathLaTeXEval` Infix 10

mathVarEntry :: MathPrimtvId -> (a->b) -> MathLaTeXEval b a
mathVarEntry name esrc
   = MathEnvd (\None -> esrc) (\None -> name) None `MathLaTeXEval` Infix 10

polyMathVarEntry :: 
      MathPrimtvId -> (a'->b) -> (forall a. BasedUpon a' a => MathLaTeXEval b a)
polyMathVarEntry name esrc
   = MathEnvd (\None -> esrc . basement) (\None -> name) None `MathLaTeXEval` Infix 10


mathExprFunction :: (a->r)
                 -> (MathPrimtvId -> MathPrimtvId)
                 -> MathLaTeXEval a c -> MathEvaluation r c
mathExprFunction f fn e = MathEnvd ( \(Identity q) -> f . q )
                                   ( fn . runIdentity )
                                   ( Identity $ contramap hHead e )
   
mathExprFn :: (a->r) -> MathPrimtvId
                 -> MathLaTeXEval a c -> MathLaTeXEval r c
mathExprFn f fn e@(MathLaTeXEval _ fxty)
   = MathLaTeXEval (mathExprFunction f funnamer e) $ Infix 9
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
    = MathLaTeXEval (mathExprInfix ifx ifxNamer el er) fxty
 where ifxNamer lexpr rexpr = mconcat
                  [ case(fxty,fxtl) of
                      (Infixl ε, Infixl κ)
                         | ε<=κ  -> plain lexpr
                      (ε, κ)
                         | isotropFixity ε<isotropFixity κ   -> plain lexpr
                         | otherwise -> parenthd lexpr
                  , " ", ifxn, " "
                  , case(fxty,fxtr) of
                      (_, RightGreedy _) -> plain rexpr
                      (Infixr ε, Infixr κ)
                         | ε<=κ  -> plain rexpr
                      (ε, κ)
                         | isotropFixity ε<isotropFixity κ   -> plain rexpr
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
  negate = mathExprFn negate ("-")
  (*) = mathExprIfx (*) (commS"cdot") $ Infixl 7
  
  signum = mathExprFn abs (mathrm"sgn")
  abs = (`MathLaTeXEval`Infix 9) . mathExprFunction abs
           (autoBrackets "|" "|")

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
lSetFold_bigSymb sumVar folderVis rngSpec summand = sumExpr `MathLaTeXEval` RightGreedy 6
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
polyLSetFold_bigSymb sumVar folderVis rngSpec summand = sumExpr `MathLaTeXEval` RightGreedy 6
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
listAsFinSet ls = listExpr `MathLaTeXEval` Infix 9
 where listExpr = MathEnvd ( const . map($HNil) )
                           ( autoBraces . mconcat . intersperse(raw",") )
                           ( map (contramap hTail) ls )


-- | Gather the results over some range. The usual @ᵢ₌₁Σ³ aᵢ⋅bᵢ@-thing.
finRFold_bigSymb :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Monoid res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> LaTeX
        -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( MathLaTeXEval rng svdStack
              -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
finRFold_bigSymb sumVar folderVis lBound uBound summand
  = sumExpr `MathLaTeXEval` RightGreedy 6
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
 
-- | Just as 'finRFold_bigSymb', but as a Rank3-function. This allows the summation-variable to
-- be used in multiple different closures, i.e. in different nesting-depths of local-sums
-- (recall that variables are type-parameterised on the entire closure).
-- However, rank>1-polymorphism cannot in general be type-infered, so you may need
-- to provide explicit signatures, which will tend to be less than beautiful. Often,
-- the simpler 'finRFold_bigSymb' will also work and should be preferred.
polyFinRFold_bigSymb :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Monoid res
              , sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> LaTeX
        -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( (forall svdStack. BasedUpon sumVarDep svdStack
                 => MathLaTeXEval rng svdStack) -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
polyFinRFold_bigSymb sumVar folderVis lBound uBound summand
  = sumExpr `MathLaTeXEval` RightGreedy 6
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
 
finRSum, finRProd :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Num res
              , BasedUpon sumVarDep svdStack, sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( MathLaTeXEval rng svdStack
              -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
finRSum sv lBG uBG = pseudoFmap getSum
                    . finRFold_bigSymb sv (TeXCommS "sum") lBG uBG
                    . (pseudoFmap Sum .)
finRProd sv lBG uBG = pseudoFmap getProduct
                    . finRFold_bigSymb sv (TeXCommS "prod") lBG uBG
                    . (pseudoFmap Product .)

polyFinRSum, polyFinRProd :: forall rng res a sumVarDep svdStack .
              ( Enum rng, Num res
              , sumVarDep ~ HCons rng a ) =>
      MathPrimtvId -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> ( (forall svdStack. BasedUpon sumVarDep svdStack
                 => MathLaTeXEval rng svdStack) -> MathLaTeXEval res sumVarDep )
          -> MathLaTeXEval res a
polyFinRSum sv lBG uBG = pseudoFmap getSum
                    . polyFinRFold_bigSymb sv (TeXCommS "sum") lBG uBG
                    . (pseudoFmap Sum .)
polyFinRProd sv lBG uBG = pseudoFmap getProduct
                    . polyFinRFold_bigSymb sv (TeXCommS "prod") lBG uBG
                    . (pseudoFmap Product .)




type LaTeXDecoratableInfix = LaTeX -> LaTeXInfix
type LaTeXInfix = LaTeX -> LaTeX -> LaTeX

makeLaTeXInfixDecoratable :: LaTeXInfix -> LaTeXDecoratableInfix
makeLaTeXInfixDecoratable ifx décor llexp rlexp = llexp <> décor `ifx` rlexp

newtype ComparisonsEval x expr
   = ComparisonsEval { runComparisonsEval :: Chain (x->x->Bool, LaTeXDecoratableInfix) expr }

instance Functor (ComparisonsEval x) where
  fmap f (ComparisonsEval ev) = ComparisonsEval $ fmap f ev
  
  
compareEnd :: (x -> x -> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> expr -> expr -> ComparisonsEval x expr -- arg
compareEnd cmp rend e 
   = ComparisonsEval . Couple e (cmp, makeLaTeXInfixDecoratable rend)

compareMid :: (x ->x -> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> expr -> ComparisonsEval x expr -> ComparisonsEval x expr -- arg
compareMid cmp rend e (ComparisonsEval c) 
   = ComparisonsEval $ chainConsl e (cmp, makeLaTeXInfixDecoratable rend) c

leftmostComparedExpr :: ComparisonsEval x expr -> expr
leftmostComparedExpr (ComparisonsEval e) = leftEnd e


infixr 4 =&, =.
class Equatable x where
  type EquateExpressionResult x :: *
  (=.) :: x -> x -> ComparisonsEval (EquateExpressionResult x) x
  (=&) :: x -> ComparisonsEval (EquateExpressionResult x) x
                   -> ComparisonsEval (EquateExpressionResult x) x
  
infixr 4 <&, <=&, >&, >=&, <., <=., >., >=.
class (Equatable x) => Orderable x where
  (<.), (<=.), (>.), (>=.) :: x -> x -> ComparisonsEval (EquateExpressionResult x) x
  (<&), (<=&), (>&), (>=&) :: x -> ComparisonsEval (EquateExpressionResult x) x
                                     -> ComparisonsEval (EquateExpressionResult x) x

infixr 4 =~&, =~.
class (Equatable x) => RoughEqable x where
  (=~.) :: x -> x -> ComparisonsEval (EquateExpressionResult x) x
  (=~&) :: x -> ComparisonsEval (EquateExpressionResult x) x
                   -> ComparisonsEval (EquateExpressionResult x) x



exprnCompareEnd :: (x->x-> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> MathLaTeXEval x arg -> MathLaTeXEval x arg
    -> ComparisonsEval x (MathLaTeXEval x arg)
exprnCompareEnd cmp rend a b
           = compareEnd cmp -- (\a b arg -> (cmp`on`(`mathExprEval`arg)) a b)
                          rComb a b
   where rComb α β = α'`rend`β'
          where [α',β'] = zipWith parenth [a,b] [α,β]
                parenth c γ | isotropFixity(mathLaTeXexprnFixity c)>4  = γ
                            | otherwise            = braces $ autoParens γ
exprnCompareMid :: (x->x-> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> MathLaTeXEval x arg -> ComparisonsEval x (MathLaTeXEval x arg)
    -> ComparisonsEval x (MathLaTeXEval x arg)
exprnCompareMid cmp rend a b
           = compareMid cmp -- (\a b arg -> (cmp`on`(`mathExprEval`arg)) a b)
                          rComb a b
   where rComb α β = α'`rend`β'
          where [α',β'] = zipWith parenth [a,leftmostComparedExpr b] [α,β]
                parenth c γ | isotropFixity(mathLaTeXexprnFixity c)>4  = γ
                            | otherwise            = braces $ autoParens γ

instance (Eq x) => Equatable(MathLaTeXEval x arg) where
  type EquateExpressionResult(MathLaTeXEval x arg) = x
--   type EquationArgument(MathLaTeXEval x arg) = arg
  (=.)  = exprnCompareEnd (==) (=:)
  (=&)  = exprnCompareMid (==) (=:)

instance (Ord x) => Orderable(MathLaTeXEval x arg) where
  (<.)  = exprnCompareEnd (<)  (<:)
  (<&)  = exprnCompareMid (<)  (<:)
  (<=.) = exprnCompareEnd (<=) (<=:)
  (<=&) = exprnCompareMid (<=) (<=:)
  (>.)  = exprnCompareEnd (>)  (>:)
  (>&)  = exprnCompareMid (>)  (>:)
  (>=.) = exprnCompareEnd (>=) (>=:)
  (>=&) = exprnCompareMid (>=) (>=:)


(≈) :: (RealFloat x) => x -> x -> Bool
a≈b = r>0.99 && r<1.01
 where r = a/b

instance (RealFloat x, e~MathLaTeXEval x arg, Equatable e) => RoughEqable e where
  (=~.) = exprnCompareEnd (≈) (between $ comm0 "approx" :: LaTeXC l => l->l->l)
  (=~&) = exprnCompareMid (≈) (between $ comm0 "approx" :: LaTeXC l => l->l->l)


infixr 8 ^*
class Num x => Powerable x where
  (^*) :: x -> x -> x
instance Powerable Int where { (^*) = (^) }
instance Powerable Double where { (^*) = (**) }
instance Powerable Float where { (^*) = (**) }
instance Powerable Integer where { (^*) = (^) }
instance (Powerable res, Show res) => Powerable (MathLaTeXEval res arg) where
  (^*) = mathExprIfx (^*) (raw"^") $ Infixr 8


instance (Fractional res) => Fractional (MathLaTeXEval res arg) where
  fromRational e = (`MathLaTeXEval`Infix 9) $ mathExprInfix (/)
           (\n d -> TeXComm "tfrac" $ map FixArg [n,d] )
           (fromIntegral $ numerator e) (fromIntegral $ denominator e)
  
  a/b = (`MathLaTeXEval`Infix 9) $ mathExprInfix (/)
           (\n d -> TeXComm "frac" $ map FixArg [n,d] ) a b
  
  recip = (`MathLaTeXEval`Infix 9) . mathExprFunction recip
           (TeXComm "frac1" . (:[]) . FixArg)

instance (Floating res) => Floating (MathLaTeXEval res arg) where
  pi = mathPrimitiv pi pi_
  
  sqrt = (`MathLaTeXEval`Infix 9) . mathExprFunction sqrt
              (TeXComm "sqrt" .(:[]). FixArg)
           
  exp = (`MathLaTeXEval`Infix 8) . mathExprFunction exp ("e" ^:)
--   b**x = (`MathLaTeXEval`Infixr 8) $ mathExprInfix (**)
  (**) = mathExprIfx (**) (raw"^") $ Infixr 8
--            (\β ξ -> T.concat [ "{", β, "}^{", ξ, "}"] ) b x
           
  log = mathExprFn log ln
  logBase b t = (`MathLaTeXEval`Infix 9) $ mathExprInfix logBase
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
  
  conjugate = (`MathLaTeXEval` Infix 10) .
                  mathExprFunction conjugate (TeXComm "overline" . (:[]) . FixArg . braces)
  realPart = mathExprFn realPart $ TeXCommS "Re"
  imagPart = mathExprFn imagPart $ TeXCommS "Im"

  magnitude = (`MathLaTeXEval`Infix 9) . mathExprFunction magnitude
           (autoBrackets "|" "|")
  phase     = mathExprFn phase $ TeXCommS "arg"
  


class MathRenderable v where
  toMathExpr :: v -> MathExpr v

instance MathRenderable Int where
  toMathExpr = fromIntegral


data RoughExpr v
  = RoughExpr { getRoughExpression :: MathExpr v }
  | ExactRoughExpr { getRoughExpression :: MathExpr v }

class MathRoughRenderable v where
  roughMathExpr :: v -> RoughExpr v

-- instance (MathRenderable v) => MathRoughRenderable v where
--   roughMathExpr = ExactRoughExpr . toMathExpr


instance MathRoughRenderable Double where
  roughMathExpr = prettyFloatApprox

instance MathRoughRenderable Integer where
  roughMathExpr = reRound . prettyFloatApprox . fromInteger
   where reRound (RoughExpr m) = RoughExpr (pseudoFmap round m)
         reRound (ExactRoughExpr m) = ExactRoughExpr (pseudoFmap round m)

instance (RealFloat r, MathRoughRenderable r) => MathRoughRenderable (Complex r) where
  roughMathExpr (a :+ b) 
    | RoughExpr a'' <- a' = RoughExpr $ a'' +| getRoughExpression b'
    | ExactRoughExpr a'' <- a' = case b' of
                                  RoughExpr b'' -> RoughExpr $ a'' +| b''
                                  ExactRoughExpr b''
                                           -> ExactRoughExpr $ a'' +| b''
   where a' = roughMathExpr a
         b' = roughMathExpr b



prettyFloatApprox :: Double -> RoughExpr Double
prettyFloatApprox x
    | (mantissa, 'e':expon) <- break(=='e') s
    , m<-read $ take 5 mantissa, expn<-read expon
    , (ExactRoughExpr mR) <- prettyFloatApprox m
                = RoughExpr $ mR * 10 ^* fromInteger expn
    | otherwise = ExactRoughExpr . mathPrimitiv x $ fromString s
 where s = remTrailing0 $ show x
       remTrailing0 = reverse . r0 . reverse
        where r0 ('0':'.':n) = n
              r0 n = n
  
mathExprEvalRough :: MathRoughRenderable v
      => MathExpr v -> RoughExpr v
mathExprEvalRough = roughMathExpr . mathExprCalculate_






inlineMathExpr :: Monad m => MathLaTeXEval b arg -> MathematicalLaTeXT m (arg->b)
inlineMathExpr e = do
   lift.lift . fromLaTeX . math $ mathExprRender e
   return $ mathExprCalculate e

inlineMathExpr_ :: Monad m => MathExpr b -> MathematicalLaTeXT m b
inlineMathExpr_ = liftM ($HNil) . inlineMathExpr


displayMathExpr :: Monad m => MathLaTeXEval b arg -> MathematicalLaTeXT m (arg->b)
displayMathExpr e = do
   stProps@(TeXMathStateProps {..}) <- get
   lift.lift . fromLaTeX . mathDisplay . srcNLEnv 
       $ mathExprRender e <> fold(fmap fromString punctuationNeededAtDisplayEnd)
   put $ stProps{ punctuationNeededAtDisplayEnd = Nothing }
   return $ mathExprCalculate e
        
       
displayMathExpr_ :: Monad m => MathExpr b -> MathematicalLaTeXT m b
displayMathExpr_ = liftM ($HNil) . displayMathExpr

inlineMathShow :: ( Monad m, MathRenderable b )
                 => b -> MathematicalLaTeXT m b
inlineMathShow = inlineMathExpr_ . toMathExpr

inlineRoughValue :: ( Monad m, MathRoughRenderable b )
                 => b -> MathematicalLaTeXT m b
inlineRoughValue = inlineMathExpr_ . getRoughExpression . roughMathExpr

infix 4 ?~?, ?=?
(?~?) :: ( Monad m, MathRoughRenderable b )
                 => String -> b -> MathematicalLaTeXT m b
expln ?~? val = fromString expln >> inlineRoughValue val
(?=?) :: ( Monad m, MathRenderable b )
                 => String -> b -> MathematicalLaTeXT m b
expln ?=? val = fromString expln >> inlineMathShow val


displayMathExpr_wRResult :: ( Monad m, MathRoughRenderable b
                            , e ~ MathExpr b, RoughEqable e  )
                   => e -> MathematicalLaTeXT m b
displayMathExpr_wRResult e = do
   let res = mathExprCalculate e HNil
   displayMathCompareSeq_ $ case roughMathExpr res of
      RoughExpr r     -> e =~. r
      ExactRoughExpr r -> e =. r
   return res

displayMathCompareSeq :: Monad m => ComparisonsEval x (MathLaTeXEval x arg)
                           -> MathematicalLaTeXT m (arg->Bool)
displayMathCompareSeq (ComparisonsEval comparisons) = do
  stProps@(TeXMathStateProps {..}) <- get
  lift.lift . fromLaTeX . align_
     $ [ renders mempty <> fold(fmap fromString punctuationNeededAtDisplayEnd) ]
  put $ stProps{ punctuationNeededAtDisplayEnd = Nothing }
  return result
 where (renders, result) = bifoldr(\(re, q) (ecs,predc)
                                     -> ((`re` ecs mempty), liftA2(&&) q predc) )
                                  (\e (ecs,predc) 
                                     -> (const . ecs $ mathExprRender e, predc) )
                                  (id, const True)
            . linksZipWith (first . flip($)) (raw"\n   &" : repeat (raw"\n \\\\ &"))
            $ linkMap (\l (cmp,re) r -> let[l',r']=map mathExprCalculate[l,r]
                                        in (re, liftA2 cmp l' r')             )
                  comparisons
                                        
displayMathCompareSeq_ :: Monad m => ComparisonsEval x (MathExpr x) 
                                                      -> MathematicalLaTeXT m Bool
displayMathCompareSeq_ = liftM ($HNil) . displayMathCompareSeq
--        go (ComparisonsEval (Middle x)) = (([r],True), (q, q))
--         where (q,r) = mathExprRender_ x
--        go (ComparisonsEval (Couple x ν )) = (([r],True), (q, q))
--        go (ExprComparison c crend l r) = ( ( lrend ++ (((crend"""")& rrendhead) : rrendtail)
--                                            , lres && rres && c lrm rlm ()                   )
--                                          , ( llm, rrm )                                      )
--         where ((lrend              , lres), (llm, lrm)) = go l
--               ((rrendhead:rrendtail, rres), (rlm, rrm)) = go r

mathDefinition :: Monad m => MathPrimtvId -> MathExpr b
                                -> MathematicalLaTeXT m(MathExpr b)
mathDefinition varn e = do
   lift.lift . fromLaTeX . math $ varn =: mathExprRender e
   return $ mathPrimitiv (mathExprCalculate_ e) varn



srcNLEnv :: LaTeX -> LaTeX
srcNLEnv e = raw"\n" <> e <> raw"\n"
                               
                               
          -- TeXMathDisplayConf should eventually contains things
          -- like the default way to render e.g. multiplication
          -- ('\cdot' vs '\times' or what environments to use.
type TeXMathConfiguration = ()
data TeXMathStateProps = TeXMathStateProps {
   punctuationNeededAtDisplayEnd :: Maybe String
 }

texMathGroundState :: TeXMathStateProps
texMathGroundState = TeXMathStateProps {
   punctuationNeededAtDisplayEnd = Nothing
 }

type MathematicalLaTeXT m a = StateT TeXMathStateProps (
                              ReaderT TeXMathConfiguration (LaTeXT m) ) a
type MathematicalLaTeXT_ m = MathematicalLaTeXT m ()  -- ReaderT TeXMathDisplayConf (LaTeXT m) ()
type MathematicalLaTeX a = MathematicalLaTeXT Identity a  -- ReaderT TeXMathDisplayConf (LaTeXT Identity) a
type MathematicalLaTeX_ = MathematicalLaTeXT Identity () -- ReaderT TeXMathDisplayConf (LaTeXT Identity) ()

instance (Monad m) => IsString (MathematicalLaTeXT m a) where
  fromString s = do
     (TeXMathStateProps {..}) <- get
     lift . lift . fromString $ case punctuationNeededAtDisplayEnd of
        Just pnct -> pnct ++ " " ++ s
        Nothing   -> s


infixr 4 ...:


-- | Request a punctuation mark to be placed at the end of (or after) the next
-- object – normally a displayed maths equation – so you can properly end a
-- sentence with maths, like
-- 
-- @
--   5 + 8 = 13.
-- @
-- 
-- To obtain such a result, use
-- 
-- @
--   do
--    \"You can properly end a sentence with maths, like\"...:\".\"
--    displayMathExpr_wRResult $ 5 + 8
-- @
-- 
-- This will not affect any result-requests you may also be conducting.
-- The fixity of this operator is @infixr 4 ...:@.
(...:) :: Monad m => MathematicalLaTeXT m a -> String -> MathematicalLaTeXT m a
txt...:punct = do
   res <- txt
   modify $ \sps -> sps{ punctuationNeededAtDisplayEnd = Just punct }
   return res

  
instance (Monad m) => Monoid (MathematicalLaTeXT_ m) where
  mempty = return()
  a`mappend`b = do
     a
     b

fromHaTeX :: Monad m => LaTeXT m a -> MathematicalLaTeXT m a
fromHaTeX = lift.lift

nl :: Monad m => MathematicalLaTeXT_ m
nl = lift.lift $ raw"\n\n"

-- instance (Monad m) => LaTeXC (MathematicalLaTeXT_ m) where
  

wDefaultConf_toHaTeX :: Monad m => MathematicalLaTeXT m a -> LaTeXT m a
wDefaultConf_toHaTeX = (`runReaderT`()) 
               . liftM fst . (`runStateT`texMathGroundState)







coFst :: a -> (a,b)
coSnd :: b -> (a,b)
coFst = (,undefined)
coSnd = (undefined,)

