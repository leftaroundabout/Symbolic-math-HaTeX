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
{-# LANGUAGE OverlappingInstances             #-}
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
  , limsSum , polyLimsSum
  , lSetSum , polyLSetSum
    -- ** Products
  , limsProd , polyLimsProd
  , lSetProd , polyLSetProd
    -- ** Generic folds
  , limsFold_bigSymb , polyLimsFold_bigSymb
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


import Math.LaTeX.Internal.MathExpr
import Math.LaTeX.RendConfig
import Math.LaTeX.Internal.RendMonad

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
import Data.Maybe(maybe)
import Data.Char(isDigit)

import Prelude hiding((^))











-- type DecoratableInfix = LaTeX -> MathLaTeXInfix 
-- 
-- makeInfixDecoratable :: MathLaTeXInfix -> DecoratableInfix
-- makeInfixDecoratable ifx décor (MathLaTeX lknd llexp) rlexp
--    = llexp <> décor `ifx` rlexp

newtype ComparisonsEval x expr
   = ComparisonsEval { runComparisonsEval :: Chain (x->x->Bool, MathLaTeXInfix) expr }

instance Functor (ComparisonsEval x) where
  fmap f (ComparisonsEval ev) = ComparisonsEval $ fmap f ev
  
  
compareEnd :: (x -> x -> Bool) -> MathLaTeXInfix
    -> expr -> expr -> ComparisonsEval x expr -- arg
compareEnd cmp rend e 
   = ComparisonsEval . Couple e (cmp, rend)

compareMid :: (x ->x -> Bool) -> MathLaTeXInfix
    -> expr -> ComparisonsEval x expr -> ComparisonsEval x expr -- arg
compareMid cmp rend e (ComparisonsEval c) 
   = ComparisonsEval $ chainConsl e (cmp, rend) c

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
exprnCompareEnd cmp rend = compareEnd cmp rComb
   where rComb α β = do
            MathLaTeXInfixAddenda{..} <- askMathLaTeXInfixAddenda
            let rend' a = rend $ a<>comparisonLineBreaker
            mathCompound_wFixity(Infixr 4) $ (rend'`on`parenth) α β
         parenth (MathLaTeX k c)
          | isotropFixityOf k > 4  = c
          | otherwise            = autoParens c
exprnCompareMid :: (x->x-> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> MathLaTeXEval x arg -> ComparisonsEval x (MathLaTeXEval x arg)
    -> ComparisonsEval x (MathLaTeXEval x arg)
exprnCompareMid cmp rend a b = compareMid cmp rComb a b
   where rComb α β = do
            MathLaTeXInfixAddenda{..} <- askMathLaTeXInfixAddenda
            let rend' a = rend $ a<>comparisonLineBreaker
            realise rend' α β
         realise r' (MathLaTeX knL lExpr) (MathLaTeX _ rExpr) = do
            mathCompound_wFixity(Infixr 4) $ r'
               ( if isotropFixityOf knL > 4
                  then noRedBraces lExpr
                  else autoParens  lExpr ) rExpr

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


class PlainRoughEqable x where
  (≈) :: x -> x -> Bool

instance (RealFloat x) => PlainRoughEqable x where
  a≈b = r>0.99 && r<1.01
   where r = a/b
 
instance (RealFloat r) => PlainRoughEqable (Complex r) where
  a≈b = ra≈rb && φΔ<0.01
   where (ra,φa) = polar a
         (rb,φb) = polar b
         φΔ = case φa - φb of
               δ | δ<pi       -> δ
                 | otherwise  -> 2*pi - δ


instance (PlainRoughEqable x, e~MathLaTeXEval x arg, Equatable e)
             => RoughEqable e where
  (=~.) = exprnCompareEnd (≈) (between $ comm0 "approx" :: LaTeXC l => l->l->l)
  (=~&) = exprnCompareMid (≈) (between $ comm0 "approx" :: LaTeXC l => l->l->l)


 


class MathRenderable v where
  toMathExpr :: v -> MathExpr v

instance MathRenderable Int where
  toMathExpr = fromIntegral


data RoughExpr v
  = RoughExpr { getRoughExpression :: MathExpr v }
  | ExactRoughExpr { getRoughExpression :: MathExpr v }

fmapRoughExpr :: (MathExpr v -> MathExpr w) -> RoughExpr v -> RoughExpr w
fmapRoughExpr f (RoughExpr q) = RoughExpr $ f q
fmapRoughExpr f (ExactRoughExpr q) = ExactRoughExpr $ f q

liftA2RoughExpr :: (MathExpr v -> MathExpr w -> MathExpr x)
                      -> RoughExpr v -> RoughExpr w -> RoughExpr x
liftA2RoughExpr f (RoughExpr q) (RoughExpr p) = RoughExpr $ f q p
liftA2RoughExpr f (RoughExpr q) (ExactRoughExpr p) = RoughExpr $ f q p
liftA2RoughExpr f (ExactRoughExpr q) p = fmapRoughExpr (f q) p

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
  roughMathExpr (a:+0) = fmapRoughExpr realAsComplex $ roughMathExpr a
  roughMathExpr (0:+b) = fmapRoughExpr ((*imagUnit) . realAsComplex) $ roughMathExpr b
  roughMathExpr (a:+1) = fmapRoughExpr ((+imagUnit) . realAsComplex) $ roughMathExpr a
  roughMathExpr (a:+(-1)) = fmapRoughExpr ((subtract imagUnit) . realAsComplex) $ roughMathExpr a
  roughMathExpr (a:+b) = rr $ \a' b' -> if b>0 then a' + b'*imagUnit
                                               else a' - b'*imagUnit
   where rr rc = (liftA2RoughExpr (flip on realAsComplex rc)
                            `on`roughMathExpr) a $ abs b
         



prettyFloatApprox :: Double -> RoughExpr Double
prettyFloatApprox x
    | (mantissa, 'e':expon) <- break(=='e') s
    , m<-read $ strRound 5 mantissa, expn<-read expon
    , (ExactRoughExpr mR) <- prettyFloatApprox m
                = RoughExpr $ mR * 10 ^ fromInteger expn
    | (intgPart, fractPt) <- break(=='.') s
    , length fractPt > 5
          = RoughExpr . mathNumPrimitiv x . fromString $ intgPart ++ strRound 4 fractPt
    | otherwise = ExactRoughExpr . mathNumPrimitiv x $ fromString s
 where s = remTrailing0 $ show x
       remTrailing0 = reverse . r0 . reverse
        where r0 ('0':'.':n) = n
              r0 n = n
       strRound n es = maybe safe (reverse . (`bckCarry` reverse safe)) o
        where (safe, o) = second (find isDigit) $ splitAt n es
              bckCarry dg ('.':rr) = '.' : bckCarry dg rr
              bckCarry dg sff
               | dg<'4'  = sff
              bckCarry _ ('9':rr) = '0' : bckCarry '6' rr
              bckCarry _ (q:rr)   = succ q : rr
  
mathExprEvalRough :: MathRoughRenderable v
      => MathExpr v -> RoughExpr v
mathExprEvalRough = roughMathExpr . mathExprCalculate_






inlineMathExpr :: Monad m => MathLaTeXEval b arg -> MathematicalLaTeXT m (arg->b)
inlineMathExpr e = do
   rendCfg <- ask
   lift.lift . fromLaTeX . math . rendrdExpression 
                  $ mathExprRender e `runReader` rendCfg
   return $ mathExprCalculate e

inlineMathExpr_ :: Monad m => MathExpr b -> MathematicalLaTeXT m b
inlineMathExpr_ = liftM ($HNil) . inlineMathExpr


displayMathExpr :: Monad m => MathLaTeXEval b arg -> MathematicalLaTeXT m (arg->b)
displayMathExpr e = do
   rendCfg <- ask
   stProps@(TeXMathStateProps {..}) <- get
   lift.lift . fromLaTeX . mathDisplay . srcNLEnv
       $  rendrdExpression (mathExprRender e `runReader` rendCfg)
           <> fold(fmap fromString punctuationNeededAtDisplayEnd)
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
  rendCfg <- ask
  let otInfixAddenda = mathLaTeXInfixAddenda rendCfg
  stProps@(TeXMathStateProps {..}) <- get
  let (result, renders) 
         = bifoldr(\(qLink, reLink) (predc, ecs)
                    -> (liftA2(&&) qLink predc, (`reLink` ecs undefined)) )
                  (\expression (predc, ecs) 
                    -> ( predc
                       , const . ecs $ runReader (mathExprRender expression) rendCfg ) )
                  (const True, id)
           . linksZipWith
                (\lineBkr -> second $ \re l r -> runReader (re l r) $
                     rendCfg{mathLaTeXInfixAddenda
                               = otInfixAddenda{comparisonLineBreaker=lineBkr} } )
                (raw"\n   &" : repeat (raw"\n \\\\ &") )
           $ linkMap (\l (cmp,re) r
                        -> let [l',r'] = map mathExprCalculate [l,r]
                           in (liftA2 cmp l' r', re)                 )
                  comparisons
  lift.lift . fromLaTeX . align_
     $ [ rendrdExpression(renders undefined)
       <> fold(fmap fromString punctuationNeededAtDisplayEnd) ]
  put $ stProps{ punctuationNeededAtDisplayEnd = Nothing }
  return result
                                       
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
   rendCfg <- ask
   lift.lift . fromLaTeX . math $ 
         varn =: rendrdExpression (mathExprRender e `runReader` rendCfg)
   return $ mathPrimitiv (mathExprCalculate_ e) varn



                               



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
  








