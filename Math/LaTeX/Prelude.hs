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
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE UndecidableInstances             #-}
{-# LANGUAGE OverlappingInstances             #-}
{-# LANGUAGE PatternGuards                    #-}
{-# LANGUAGE TypeFamilies                     #-}
{-# LANGUAGE RankNTypes                       #-}
{-# LANGUAGE ImpredicativeTypes               #-}
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
  , displayMathExpr_wRResultAsTypeOf
  , displayMathCompareSeq , displayMathCompareSeq_
  , mathExprEvalRough
    -- * Construction
  , mathPrimitiv , mathDepPrimitiv
  , mathExprFunction, mathExprFn
  , mathExprInfix, mathExprIfx
  , mathDefinition, mathFuncDefinition
  , prettyFloatApprox
    -- * Manual tweaking of math expression displays
  , forceParens, unsafeOmitParens, manBracketSize
    -- * Equivalency-relation classes
  , Equatable(..)
  , Orderable(..)
  , RoughEqable(..)
    -- * Misc
  , Powerable(..)
  , ($$$), ($=$), ($$!), ($:$)
  , BasedUpon
  , freeVarIntro, NewFreeVar
    -- * The rendering monad
  , MathematicalLaTeX, MathematicalLaTeX_
  , MathematicalLaTeXT, MathematicalLaTeXT_
  , toHaTeX_wConfig
  , wDefaultConf_toHaTeX
  , fromHaTeX
  , nl
  ) where


import Math.LaTeX.Internal.MathExpr
import Math.LaTeX.Config
import Math.LaTeX.Internal.RendMonad

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import qualified Data.Text as T

import Text.Printf

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
import Data.Maybe(maybe, catMaybes)
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

instance PlainRoughEqable Integer where
  (≈) = (==)

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
  roughMathExpr = prettyFloatApprox 3

instance MathRoughRenderable Integer where
  roughMathExpr = reRound . prettyFloatApprox 3 . fromInteger
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
         



prettyFloatApprox :: Int -> Double -> RoughExpr Double
prettyFloatApprox _ 0 = ExactRoughExpr 0
prettyFloatApprox preci x
    | x < 0 = case prettyFloatApprox preci (-x) of
               ExactRoughExpr q -> ExactRoughExpr $ -q
               RoughExpr q      -> RoughExpr      $ -q
    | x < 10^^(preci+2), Just x_i <- maybeIntCast x  = ExactRoughExpr $ fromInteger x_i
    | ((ngExp, x'_i):_) <- catMaybes $ map (\(e, x') -> fmap (e,) $ maybeIntCast x') multiples
          = ExactRoughExpr $ if abs ngExp > 3 
                              then fromInteger x'_i * 10 ^ fromIntegral (-ngExp)
                              else realToFrac $ fromInteger x'_i * 10 ^^ (-ngExp)
    | preci < 1 = RoughExpr $ 10 ^ fromIntegral e₀
    | x < 1000, x > 0.1, m <- 10^^preci, x' <- fromIntegral(round $ x*m) / m
          = RoughExpr . mathNumPrimitiv x' . fromString . rmLead0 
                $ printf ("%."++show (preci-e₀)++"g") x'
    | RoughExpr mantissa <- prettyFloatApprox (max 1 $ preci-1) $ x / 10^^e₀
          = RoughExpr $ mantissa * 10 ^ fromIntegral e₀
 where a ≈ b = abs (a - b) < ε
        where ε = minimum $ map ((* 1e-10) . abs) [a, b]
       maybeIntCast a | b<-round a, a ≈ fromIntegral b  = Just b
                      | otherwise                       = Nothing
       multiples = [ (e, x * 10^^e) | e <- [-e₀ .. preci - e₀] ]
       e₀ = floor $ log x / log 10
       rmLead0 ('0':s) = s
       rmLead0 s = s
   
   
  
mathExprEvalRough :: MathRoughRenderable v
      => MathExpr v -> RoughExpr v
mathExprEvalRough = roughMathExpr . mathExprCalculate_






inlineMathExpr :: Monad m => MathLaTeXEval b arg -> MathematicalLaTeXT arg m (arg->b)
inlineMathExpr e = do
   rendCfg <- ask
   fromHaTeX . fromLaTeX . math . rendrdExpression 
                  $ mathExprRender e `runReader` rendCfg
   return $ mathExprCalculate e

inlineMathExpr_ :: Monad m => MathExpr b -> MathematicalLaTeXT HNil' m b
inlineMathExpr_ = liftM ($HNil') . inlineMathExpr


displayMathExpr :: Monad m => MathLaTeXEval b arg -> MathematicalLaTeXT arg m (arg->b)
displayMathExpr e = do
   rendCfg <- ask
   stProps@(TeXMathStateProps {..}) <- get
   fromHaTeX . fromLaTeX . mathDisplay . srcNLEnv
       $  rendrdExpression (mathExprRender e `runReader` rendCfg)
           <> fold(fmap fromString punctuationNeededAtDisplayEnd)
   put $ stProps{ punctuationNeededAtDisplayEnd = Nothing }
   return $ mathExprCalculate e
        
       
displayMathExpr_ :: Monad m => MathExpr b -> MathematicalLaTeXT HNil' m b
displayMathExpr_ = liftM ($HNil') . displayMathExpr

inlineMathShow :: ( Monad m, MathRenderable b )
                 => b -> MathematicalLaTeXT HNil' m b
inlineMathShow = inlineMathExpr_ . toMathExpr

inlineRoughValue :: ( Monad m, MathRoughRenderable b )
                 => b -> MathematicalLaTeXT HNil' m b
inlineRoughValue = inlineMathExpr_ . getRoughExpression . roughMathExpr

infix 4 ?~?, ?=?
(?~?) :: ( Monad m, MathRoughRenderable b )
                 => String -> b -> MathematicalLaTeXT HNil' m b
expln ?~? val = fromString expln >> inlineRoughValue val
(?=?) :: ( Monad m, MathRenderable b )
                 => String -> b -> MathematicalLaTeXT HNil' m b
expln ?=? val = fromString expln >> inlineMathShow val


-- | Display a math expression, together with its calculated result
-- (exactly if feasible, approximate otherwise – in either case, the
-- appropriate equality symbol will be chosen).
displayMathExpr_wRResult :: ( Monad m, MathRoughRenderable b
                            , e ~ MathExpr b, RoughEqable e  )
                   => e -> MathematicalLaTeXT HNil' m b
displayMathExpr_wRResult e = do
   let res = mathExprCalculate e HNil'
   displayMathCompareSeq_ $ case roughMathExpr res of
      RoughExpr r     -> e =~. r
      ExactRoughExpr r -> e =. r
   return res

-- | Same as 'displayMathExpr_wRResult', but takes an extra dummy parameter
-- which can be used to determine what type the calculated expression is
-- supposed to have.
displayMathExpr_wRResultAsTypeOf :: ( Monad m, MathRoughRenderable b
                            , e ~ MathExpr b, RoughEqable e  )
                   => b -> e -> MathematicalLaTeXT HNil' m b
displayMathExpr_wRResultAsTypeOf _ = displayMathExpr_wRResult
 

displayMathCompareSeq :: Monad m => ComparisonsEval x (MathLaTeXEval x arg)
                           -> MathematicalLaTeXT arg m (arg->Bool)
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
  fromHaTeX . fromLaTeX . align_
     $ [ rendrdExpression(renders undefined)
       <> fold(fmap fromString punctuationNeededAtDisplayEnd) ]
  put $ stProps{ punctuationNeededAtDisplayEnd = Nothing }
  return result
                                       
                                       
displayMathCompareSeq_ :: Monad m => ComparisonsEval x (MathExpr x) 
                                                      -> MathematicalLaTeXT HNil' m Bool
displayMathCompareSeq_ = liftM ($HNil') . displayMathCompareSeq


-- mathDefinition :: Monad m => MathPrimtvId -> MathLaTeXEval a b
--                                 -> MathematicalLaTeXT b m (MathLaTeXEval a b)
mathDefinition :: forall m x a . (Monad m)
     => MathPrimtvId -> MathLaTeXEval x a
           -> MathematicalLaTeXT a m ( forall a' . BasedUpon a a'
                                            => MathLaTeXEval x a' )
mathDefinition varn e = do
   rendCfg <- ask
   fromHaTeX . fromLaTeX . math $ 
         varn =: rendrdExpression (mathExprRender e `runReader` rendCfg)
   (return :: (forall a' . BasedUpon a a' => MathLaTeXEval x a')
               -> (MathematicalLaTeXT a m ( forall a' . BasedUpon a a' => MathLaTeXEval x a' ) ) )
       $ polyMathVarEntry varn (mathExprCalculate e)


mathFuncDefinition :: forall m fnarg res a .
    (Monad m)
     => MathPrimtvId -> MathPrimtvId
            -> ( (forall a' . BasedUpon a a'
                   => MathLaTeXEval fnarg a') -> MathLaTeXEval res a)
           -> MathematicalLaTeXT a m ( forall a' . BasedUpon a a'
                                       => MathLaTeXEval (fnarg->res) a' )
mathFuncDefinition funcn varn ef = do
   rendCfg <- ask
   let fnSymbExpr :: forall a' . BasedUpon a a' => MathLaTeXEval (fnarg->res) a'
       fnSymbExpr = polyMathVarEntry funcn 
                     (\a v -> (`mathExprCalculate`a) $ ef (mathVarEntry varn $ const v))
       varSymbExpr :: forall a' . BasedUpon a a' => MathLaTeXEval fnarg a'
       varSymbExpr = mathPrimitiv undefined varn
       dqRenderer e = mathExprRender e `runReader` rendCfg
   fromHaTeX . fromLaTeX . math $
      rendrdExpression (dqRenderer $ fnSymbExpr $$$ varSymbExpr)
        =: rendrdExpression (dqRenderer $ ef varSymbExpr)
   (return :: (forall a' . BasedUpon a a' => MathLaTeXEval (fnarg->res) a')
             -> (MathematicalLaTeXT a m ( forall a' . BasedUpon a a'
                                       => MathLaTeXEval (fnarg->res) a' )) ) fnSymbExpr

-- MathematicalLaTeXT a m (
   
                               
type NewFreeVar v = forall outerFree innerFree m mRe .
         ( Monad m, innerFree ~ HCons' v outerFree )
      => ( ( forall c . BasedUpon innerFree c => MathLaTeXEval v c )
         -> MathematicalLaTeXT innerFree m mRe )
     -> MathematicalLaTeXT outerFree m mRe



freeVarIntro :: forall v outerFree innerFree m mRe .
         ( Monad m, innerFree ~ HCons' v outerFree )
  => MathPrimtvId
    -> ( ( forall c . BasedUpon innerFree c => MathLaTeXEval v c )
         -> MathematicalLaTeXT innerFree m mRe )
     -> MathematicalLaTeXT outerFree m mRe
freeVarIntro vn qgen = tamperFreeVarStack $ qgen vVar
 where vVar :: forall c . BasedUpon innerFree c => MathLaTeXEval v c
       vVar = mathVarEntry vn (\c -> hHead (base c))
        where base :: c -> innerFree; base = basement



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
(...:) :: Monad m => MathematicalLaTeXT f m a -> String -> MathematicalLaTeXT f m a
txt...:punct = do
   res <- txt
   modify $ \sps -> sps{ punctuationNeededAtDisplayEnd = Just punct }
   return res

  
instance (Monad m) => Monoid (MathematicalLaTeXT_ m) where
  mempty = return()
  a`mappend`b = do
     a
     b


nl :: Monad m => MathematicalLaTeXT f m a
nl = fromHaTeX $ raw"\n\n"

-- instance (Monad m) => LaTeXC (MathematicalLaTeXT_ m) where
  








