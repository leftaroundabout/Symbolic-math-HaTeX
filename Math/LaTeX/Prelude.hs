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
{-# LANGUAGE TupleSections                    #-}
{-# LANGUAGE RecordWildCards                  #-}

module Math.LaTeX.Prelude ( -- * Data types
                            MathLaTeXEval
                          , MathExpr
                          , ComparisonsEval
                            -- * Adaptions of arithmetic calculations
                          , finRSum, lSetSum, listAsFinSet
                            -- * Rendering
                          , mathExprRender
                          , mathExprCalculate , mathExprCalculate_
                          , inlineMathExpr , inlineMathExpr_ , inlineRoughValue
                          , displayMathExpr , displayMathExpr_
                          , displayMathExpr_wRResult
                          , displayMathCompareSeq , displayMathCompareSeq_
                          , mathExprEvalRough
                            -- * Construction
                          , mathPrimitiv
                          , mathExprFunction, mathExprFn
                          , mathExprInfix, mathExprIfx
                          , mathDefinition
                          , prettyFloatApprox
                            -- * Additional classes, for generic operations where
                            --   the standard classes include fixed types and can
                            --   therefore not be used to generate LaTeX math.
                          , Equatable(..)
                          , Orderable(..)
--                           , MagnitudeOrd(..)
                          , Powerable(..)
                            -- * The rendering monad
                          , MathematicalLaTeX, MathematicalLaTeX_
                          , MathematicalLaTeXT, MathematicalLaTeXT_
                          , wDefaultTeXMathDisplayConf
                          ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity

import Data.List
import Data.Chain
import Data.Function
import Data.Ratio
import Data.Functor.Contravariant
import Data.Bifunctor
import Data.Bifoldable
import Data.String







type MathPrimtvId = LaTeX
data Fixity = Infix Int
            | Infixl Int
            | Infixr Int
isotropFixity :: Fixity -> Int
isotropFixity (Infix n) = n
isotropFixity (Infixl n) = n
isotropFixity (Infixr n) = n

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
           , enclosedMathExpr :: list(MathLaTeXEval b (a,c))
           } -> MathEvaluation d c

data MathLaTeXEval res arg
      = MathLaTeXEval { mathLaTeXevaluation :: MathEvaluation res arg 
                      , mathLaTeXexprnFixity :: Fixity
                      }

instance Contravariant (MathEvaluation res) where
  contramap f(MathEnvd g wr encld) = MathEnvd g' wr encld'
     where g' l = g l . f
           encld' = fmap(contramap $ \(a,c) -> (a,f c)) encld
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



withArg :: a -> MathLaTeXEval res a -> MathLaTeXEval res ()
withArg = contramap . const

type MathExpr a = MathLaTeXEval a ()


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
                    -> calculated e' (a,c) ) $ enclosed) c

mathExprCalculate_ :: MathLaTeXEval b () -> b
mathExprCalculate_ x = mathExprCalculate x ()

mathPrimitiv :: b -> LaTeX -> MathLaTeXEval b a
-- mathPrimitiv v name = MathLaTeXEval (MathPrimitive v name) 10
mathPrimitiv v name
  = (MathEnvd (\None _->v) (\None -> name) None) `MathLaTeXEval` Infix 10

mathVarEntry :: MathPrimtvId -> (a->b) -> MathLaTeXEval b a
mathVarEntry name esrc
   = MathEnvd (\None -> esrc) (\None -> name) None `MathLaTeXEval` Infix 10


mathExprFunction :: (a->r)
                 -> (MathPrimtvId -> MathPrimtvId)
                 -> MathLaTeXEval a c -> MathEvaluation r c
mathExprFunction f fn e = MathEnvd ( \(Identity q) -> f . q )
                                   ( fn . runIdentity )
                                   ( Identity $ contramap fst e )
   
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
             ( Pair (contramap fst el) (contramap fst er) )
             
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
             ( Pair ( pseudoFmap(,undefined) $ contramap fst el )
                    ( pseudoFmap(undefined,) $ contramap fst er ) )


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
                                   

instance (Num res, Show res) => Num (MathLaTeXEval res arg) where
  fromInteger n = mathPrimitiv (fromInteger n) (rendertex n)
  
  (+) = mathExprIfx (+) "+" $ Infixl 6
  (-) = mathExprIfx (-) "-" $ Infixl 6
  negate = mathExprFn negate ("-")
  (*) = mathExprIfx (*) (commS"cdot") $ Infixl 7
  
  signum = mathExprFn abs (mathrm"sgn")
  abs = (`MathLaTeXEval`Infix 9) . mathExprFunction abs
           (autoBrackets "|" "|")

-- instance (Enum r, Show r) => Enum (MathExpr



lSetSum :: forall rng res a. Num res 
       => MathPrimtvId -> MathLaTeXEval [rng] a
                 -> (MathLaTeXEval rng (rng,a) -> MathLaTeXEval res (rng,a))
                 -> MathLaTeXEval res a
lSetSum sumVar rngSpec summand = sumExpr `MathLaTeXEval` Infix 6
 where sumExpr 
        = MathEnvd (                                    (
                     \(Pair rngG summandG) _ -> 
                         sum [ fst $ summandG x
                          | x<-snd $ rngG undefined ]
                                              ) :: Pair(rng -> (res, [rng])) -> a -> res )
                   ( \(Pair rngV summandV) -> 
                          (TeXCommS "sum" !: braces(sumVar `in_` rngV)) <> summandV 
                               )
                   ( Pair ( pseudoFmap coSnd $ contramap snd rngSpec )
                          ( pseudoFmap coFst
                                . summand $ mathVarEntry sumVar fst )
                                              :: Pair(MathLaTeXEval (res, [rng]) (rng, a) ) )

-- | A list only represents a set properly when there are no duplicate elements,
-- a precondition which this function doesn't (and can't!) check.
listAsFinSet :: [MathLaTeXEval r a] -> MathLaTeXEval [r] a
listAsFinSet ls = listExpr `MathLaTeXEval` Infix 9
 where listExpr = MathEnvd ( const . map($()) )
                           ( autoBraces . mconcat . intersperse(raw",") )
                           ( map (contramap snd) ls )


-- | Sum over some range. The usual @ᵢ₌₁Σ³ aᵢ⋅bᵢ@-thing.
finRSum :: (Enum rng, Num res) =>
      MathPrimtvId -> MathLaTeXEval rng a -> MathLaTeXEval rng a
          -> (MathLaTeXEval rng (rng,a) -> MathLaTeXEval res (rng,a))
          -> MathLaTeXEval res a
finRSum sumVar lBound uBound summand
  = sumExpr `MathLaTeXEval` Infix 9
 where sumExpr = MathEnvd ( \(Triple rngLG rngUG summandG) _ ->
                               sum [ fst $ summandG x
                                | x<-[snd $ rngLG undefined .. snd $ rngUG undefined] ] ) 
                          ( \(Triple rngLV rngUV summandV) ->
                                (TeXCommS "sum" !: braces(sumVar =: rngLV)
                                                ^: braces(rngUV)        ) <> summandV )
                          ( Triple (pseudoFmap coSnd $ contramap snd lBound )
                                   (pseudoFmap coSnd $ contramap snd uBound )
                                   (pseudoFmap coFst . summand
                                      $ mathVarEntry sumVar fst                     ) )
                         




newtype ComparisonsEval x expr
   = ComparisonsEval { runComparisonsEval :: Chain (x->x->Bool, LaTeX->LaTeX->LaTeX) expr }

instance Functor (ComparisonsEval x) where
  fmap f (ComparisonsEval ev) = ComparisonsEval $ fmap f ev
-- instance Contravariant (ComparisonsEval x expr) where
--   contramap f(ComparisonsEval c) = ComparisonsEval c'
--    where c' = linkMap (\(pr,re)->(\x y arg->pr x y $ f arg, re)) c

-- data ComparisonsEval x expr arg
--  = ExprComparison { exprnComparisonChain 
--                       :: NonEmpty(expr, (x->x->arg->Bool, LaTeX->LaTeX->LaTeX))
--                              -- alternating: the middle element of the comparison chain is at the end of the list. The comparator is WRT the element to the right.
--                   , exprnComparisonTerminate :: expr }
-- instance Contravariant (ComparisonsEval x expr) where
--   contramap f (ExprComparison mids r) = ExprComparison
--                   (fmap(\(e,q,b)->(e, \x y arg->q x y$f arg, b)) mids) r

-- type Comparisons x expr = ComparisonsEval x expr ()
                                        
compareEnd :: (x -> x -> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> expr -> expr -> ComparisonsEval x expr -- arg
compareEnd cmp rend e = ComparisonsEval . Couple e (cmp,rend)
-- compareEnd cmp rend e = ExprComparison $ Eventually(e, (cmp, rend))

compareMid :: (x ->x -> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> expr -> ComparisonsEval x expr -> ComparisonsEval x expr -- arg
compareMid cmp rend e (ComparisonsEval c) = ComparisonsEval $ chainConsl e (cmp,rend) c
-- compareMid cmp rend e (ExprComparison) = ExprComparison cmp rend . ExprToCompare

-- compareEnd_ :: (x->x->Bool) -> (LaTeX->LaTeX->LaTeX) -> expr -> expr -> ComparisonsEval x expr ()
-- compareEnd_ cmp = compareEnd (\l r ()->cmp l r)
-- compareMid_ :: (x->x->Bool) -> (LaTeX->LaTeX->LaTeX)
--     -> expr -> ComparisonsEval x expr () -> ComparisonsEval x expr ()
-- compareMid_ cmp = compareMid (\l r ()->cmp l r)

leftmostComparedExpr :: ComparisonsEval x expr -> expr
leftmostComparedExpr (ComparisonsEval e) = leftEnd e
-- leftmostComparedExpr (ExprComparison _ _ l _) = leftmostComparedExpr l


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

-- instance (Eq x) => Equatable x where
--   type EquateExpressionResult x = x
--   (=.)  = compareEnd_ (==) (=:)
--   (=&)  = compareMid_ (==) (=:)
--   
-- instance (Ord x) => Orderable x where
--   (<.)  = compareEnd_ (<)  (<:)
--   (<&)  = compareMid_ (<)  (<:)
--   (<=.) = compareEnd_ (<=) (<=:)
--   (<=&) = compareMid_ (<=) (<=:)
--   (>.)  = compareEnd_ (>)  (>:)
--   (>&)  = compareMid_ (>)  (>:)
--   (>=.) = compareEnd_ (>=) (>=:)
--   (>=&) = compareMid_ (>=) (>=:)


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


instance (Fractional res, Show res) => Fractional (MathLaTeXEval res arg) where
  fromRational e = (`MathLaTeXEval`Infix 9) $ mathExprInfix (/)
           (\n d -> TeXComm "tfrac" $ map FixArg [n,d] )
           (fromIntegral $ numerator e) (fromIntegral $ denominator e)
  
  a/b = (`MathLaTeXEval`Infix 9) $ mathExprInfix (/)
           (\n d -> TeXComm "frac" $ map FixArg [n,d] ) a b
  
  recip = (`MathLaTeXEval`Infix 9) . mathExprFunction recip
           (TeXComm "frac1" . (:[]) . FixArg)

instance (Floating res, Show res) => Floating (MathLaTeXEval res arg) where
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

-- instance MathRoughRenderable Integer where
--   roughMathExpr = prettyFloatApprox . fromInteger



prettyFloatApprox :: Double -> RoughExpr Double
prettyFloatApprox x
    | (mantissa, 'e':expon) <- break(=='e') s
    , m<-read $ take 7 mantissa, expn<-read expon
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
   lift . fromLaTeX . math $ mathExprRender e
   return $ mathExprCalculate e

inlineMathExpr_ :: Monad m => MathExpr b -> MathematicalLaTeXT m b
inlineMathExpr_ = liftM ($()) . inlineMathExpr

displayMathExpr :: Monad m => MathLaTeXEval b arg -> MathematicalLaTeXT m (arg->b)
displayMathExpr e = do
   lift . fromLaTeX . mathDisplay $ mathExprRender e
   return $ mathExprCalculate e
       
displayMathExpr_ :: Monad m => MathExpr b -> MathematicalLaTeXT m b
displayMathExpr_ = liftM ($()) . displayMathExpr

inlineRoughValue :: ( Monad m, MathRoughRenderable b )
                 => b -> MathematicalLaTeXT m b
inlineRoughValue = inlineMathExpr_ . getRoughExpression . roughMathExpr

displayMathExpr_wRResult :: ( Monad m, MathRoughRenderable b
                            , e ~ MathExpr b, RoughEqable e  )
                   => e -> MathematicalLaTeXT m b
displayMathExpr_wRResult e = do
   let res = mathExprCalculate e ()
   displayMathCompareSeq_ $ case roughMathExpr res of
      RoughExpr r     -> e =~. r
      ExactRoughExpr r -> e =. r
   return res

displayMathCompareSeq :: Monad m => ComparisonsEval x (MathLaTeXEval x arg)
                           -> MathematicalLaTeXT m (arg->Bool)
displayMathCompareSeq (ComparisonsEval comparisons) = do
  lift . fromLaTeX . align_ $ inlineFirst renders
  return result
 where inlineFirst (r1:r2:rs) = r1<>r2 : rs
       inlineFirst rs = rs
       (renders, result) = bifoldr(\(re, q) (ex:ecs,predc)
                                          -> (raw"&"`re`ex : ecs, liftA2(&&) q predc) )
                                  (\e (ecs,predc) -> (mathExprRender e:ecs, predc) )
                                  ([], const True)
            $ linkMap (\l (cmp,re) r -> let[l',r']=map mathExprCalculate[l,r]
                                        in (re, liftA2 cmp l' r'))
                  comparisons
                                        
displayMathCompareSeq_ :: Monad m => ComparisonsEval x (MathExpr x) 
                                                      -> MathematicalLaTeXT m Bool
displayMathCompareSeq_ = liftM ($()) . displayMathCompareSeq
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
   lift . fromLaTeX . math $ varn =: mathExprRender e
   return $ mathPrimitiv (mathExprCalculate_ e) varn

                               
                               
          -- TeXMathDisplayConf should eventually contains things
          -- like the default way to render e.g. multiplication
          -- ('\cdot' vs '\times' or what environments to use.
type TeXMathDisplayConf = ()


type MathematicalLaTeXT m a = ReaderT TeXMathDisplayConf (LaTeXT m) a
type MathematicalLaTeXT_ m = MathematicalLaTeXT m ()  -- ReaderT TeXMathDisplayConf (LaTeXT m) ()
type MathematicalLaTeX a = MathematicalLaTeXT Identity a  -- ReaderT TeXMathDisplayConf (LaTeXT Identity) a
type MathematicalLaTeX_ = MathematicalLaTeXT Identity () -- ReaderT TeXMathDisplayConf (LaTeXT Identity) ()

instance (Monad m) => IsString (MathematicalLaTeXT m a) where
  fromString s = lift $ fromString s
  
instance (Monad m) => Monoid (MathematicalLaTeXT_ m) where
  mempty = return()
  a`mappend`b = do
     a
     b

-- instance (Monad m) => LaTeXC (MathematicalLaTeXT_ m) where
  

wDefaultTeXMathDisplayConf = (`runReaderT`())







-- Some trivial fixed-size-array functors, these model the structural recursion of
-- either primitive math (None), unary functions (Identity), or infix functions (Pair).
data None a = None
instance Functor None where { fmap _ None = None }
data Pair a = Pair a a
instance Functor Pair where { fmap f (Pair l r) = Pair (f l) (f r) }
data Triple a = Triple a a a
instance Functor Triple where { fmap f (Triple l m r) = Triple (f l) (f m) (f r) }


coFst :: a -> (a,b)
coSnd :: b -> (a,b)
coFst = (,undefined)
coSnd = (undefined,)

