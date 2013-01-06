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

module Math.LaTeX.Prelude ( -- * Data types
                            MathLaTeXEval
                          , MathExpr
                          , ComparisonsEval
                          , Comparisons
                            -- * Rendering
                          , mathExprRender
                          , inlineMathExpr
                          , displayMathExpr
                          , displayMathCompareSeq
                          , wDefaultTeXMathDisplayConf
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
                          ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Function
import Data.Ratio
import Data.Functor.Contravariant
import Data.String



-- Some trivial fixed-size-array functors, these model the structural recursion of
-- either primitive math (None), unary functions (Identity), or infix functions (Pair).
data None a = None
instance Functor None where { fmap _ None = None }
data Pair a = Pair a a
instance Functor Pair where { fmap f (Pair l r) = Pair (f l) (f r) }




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
           , argToEnclosed :: MathPrimtvId
           , enclosingLaTeX :: list LaTeX -> LaTeX
           , enclosedMathExpr :: MathPrimtvId -> list(MathLaTeXEval b (a,c))
           } -> MathEvaluation d c

data MathLaTeXEval res arg
      = MathLaTeXEval { mathLaTeXevaluation :: MathEvaluation res arg 
                      , mathLaTeXexprnFixity :: Fixity
                      }

instance Contravariant (MathLaTeXEval res) where
  contramap f (MathLaTeXEval e fxty) = MathLaTeXEval (cmap f e) fxty
   where cmap :: forall c c' d. (c->c') -> MathEvaluation d c' -> MathEvaluation d c
         cmap f(MathEnvd g ai wr encld) = MathEnvd g' ai wr encld'
          where g' l = g l . f
                encld' = fmap(contramap $ \(a,c) -> (a,f c)) . encld

withArg :: a -> MathLaTeXEval res a -> MathLaTeXEval res ()
withArg = contramap . const

type MathExpr a = MathLaTeXEval a ()


mathExprRender :: MathLaTeXEval b arg -> arg -> (b, LaTeX)
mathExprRender (MathLaTeXEval e _) arg = (calculated e arg, rendered e)
 where calculated :: MathEvaluation d c -> c -> d
       calculated (MathEnvd f arg _ enclosed) c
            = f (fmap(\(MathLaTeXEval e' _) a
                               -> calculated e' (a,c) ) $ enclosed arg) c
--        rendered (MathPrimitive _ txm) = txm
--        rendered (MathDepdPrimitive _ txf) = txf i
       rendered :: MathEvaluation c a -> LaTeX
       rendered (MathEnvd _ a txf enclosed)
            = txf . fmap(rendered . mathLaTeXevaluation) $ enclosed a

mathExprRender_ :: MathLaTeXEval b () -> (b, LaTeX)
mathExprRender_ x = mathExprRender x ()

mathExprEval :: MathLaTeXEval b arg -> arg -> b
mathExprEval x = fst . mathExprRender x

mathPrimitiv :: b -> LaTeX -> MathLaTeXEval b a
-- mathPrimitiv v name = MathLaTeXEval (MathPrimitive v name) 10
mathPrimitiv v name
  = MathLaTeXEval (MathEnvd (\None _->v) "" (const name) (const None)) $ Infix 10



mathExprFunction :: (a->r)
                 -> (MathPrimtvId -> MathPrimtvId)
                 -> MathLaTeXEval a c -> MathEvaluation r c
mathExprFunction f fn e = MathEnvd ( \(Identity q) -> f . q )
                                   ( "" )
                                   ( fn . runIdentity )
                                   ( const . Identity $ contramap fst e )
   
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
             ( "" )
             ( \(Pair q p) -> ifxn q p )
             ( const $ Pair (contramap fst el) (contramap fst er) )
             
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
  (*) = mathExprIfx (*) (commS"cdot") $ Infixl 7
  
  signum = mathExprFn abs (mathrm"sgn")
  abs = (`MathLaTeXEval`Infix 9) . mathExprFunction abs
           (autoBrackets "|" "|")





         -- Perhaps a linear structure would actually be sufficient.
data ComparisonsEval x expr arg = ExprToCompare expr
                                | ExprComparison (x -> x -> arg -> Bool)
                                                 (LaTeX->LaTeX -> LaTeX)
                                                 (ComparisonsEval x expr arg)
                                                 (ComparisonsEval x expr arg)
instance Contravariant (ComparisonsEval x expr) where
  contramap _ (ExprToCompare x) = ExprToCompare x
  contramap f x@(ExprComparison c rend l r) = ExprComparison c' rend l' r'
   where c' le re q = c le re $ f q
         [l', r'] = map (contramap f) [l,r]

type Comparisons x = ComparisonsEval x ()
                                        
compareEnd :: (x -> x -> arg -> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> expr -> expr -> ComparisonsEval x expr arg
compareEnd cmp rend = ExprComparison cmp rend `on` ExprToCompare
compareMid :: (x ->x -> arg -> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> expr -> ComparisonsEval x expr arg -> ComparisonsEval x expr arg
compareMid cmp rend = ExprComparison cmp rend . ExprToCompare

compareEnd_ :: (x->x->Bool) -> (LaTeX->LaTeX->LaTeX) -> expr -> expr -> ComparisonsEval x expr ()
compareEnd_ cmp = compareEnd (\l r ()->cmp l r)
compareMid_ :: (x->x->Bool) -> (LaTeX->LaTeX->LaTeX)
    -> expr -> ComparisonsEval x expr () -> ComparisonsEval x expr ()
compareMid_ cmp = compareMid (\l r ()->cmp l r)

leftmostComparedExpr :: ComparisonsEval x expr arg -> expr
leftmostComparedExpr (ExprToCompare e) = e
leftmostComparedExpr (ExprComparison _ _ l _) = leftmostComparedExpr l



infixr 4 =&, =.
class Equatable x where
  type EquateExpressionResult x :: *
  type EquationArgument x :: *
  (=.) :: x -> x -> ComparisonsEval (EquateExpressionResult x) x (EquationArgument x)
  (=&) :: x -> ComparisonsEval (EquateExpressionResult x) x (EquationArgument x)
                   -> ComparisonsEval (EquateExpressionResult x) x (EquationArgument x)
  
infixr 4 <&, <=&, >&, >=&, <., <=., >., >=.
class (Equatable x) => Orderable x where
  (<.), (<=.), (>.), (>=.) :: x -> x -> ComparisonsEval (EquateExpressionResult x) x (EquationArgument x)
  (<&), (<=&), (>&), (>=&) :: x -> ComparisonsEval (EquateExpressionResult x) x (EquationArgument x)
                                     -> ComparisonsEval (EquateExpressionResult x) x (EquationArgument x)

-- instance (Eq x) => Equatable x where
--   type EquationArgument x = ()
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
    -> ComparisonsEval x (MathLaTeXEval x arg) arg
exprnCompareEnd cmp rend a b
           = compareEnd (\a b arg -> (cmp`on`(`mathExprEval`arg)) a b) rComb a b
   where rComb α β = α'`rend`β'
          where [α',β'] = zipWith parenth [a,b] [α,β]
                parenth c γ | isotropFixity(mathLaTeXexprnFixity c)>4  = γ
                            | otherwise            = braces $ autoParens γ
exprnCompareMid :: (x->x-> Bool) -> (LaTeX->LaTeX->LaTeX)
    -> MathLaTeXEval x arg -> ComparisonsEval x (MathLaTeXEval x arg) arg
    -> ComparisonsEval x (MathLaTeXEval x arg) arg
exprnCompareMid cmp rend a b
           = compareMid (\a b arg -> (cmp`on`(`mathExprEval`arg)) a b) rComb a b
   where rComb α β = α'`rend`β'
          where [α',β'] = zipWith parenth [a,leftmostComparedExpr b] [α,β]
                parenth c γ | isotropFixity(mathLaTeXexprnFixity c)>4  = γ
                            | otherwise            = braces $ autoParens γ

instance (Eq x) => Equatable(MathLaTeXEval x arg) where
  type EquateExpressionResult(MathLaTeXEval x arg) = x
  type EquationArgument(MathLaTeXEval x arg) = arg
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
  

prettyFloatApprox :: Double -> MathExpr Double
prettyFloatApprox x
    | (mantissa, e:expon) <- break(=='e') s
    , m<-read $ take 7 mantissa, expn<-read expon
                = prettyFloatApprox m * 10 ^* fromInteger expn
    | otherwise = mathPrimitiv x $ fromString s
 where s = remTrailing0 $ show x
       remTrailing0 = reverse . r0 . reverse
        where r0 ('0':'.':n) = n
              r0 n = n
  



inlineMathExpr :: Monad m => MathExpr b -> MathematicalLaTeXT m b
inlineMathExpr e = do
   lift . fromLaTeX $ math rendered
   return result
 where (result, rendered) = mathExprRender_ e

displayMathExpr :: Monad m => MathExpr b -> MathematicalLaTeXT m b
displayMathExpr e = do
   lift . fromLaTeX $ mathDisplay rendered
   return result
 where (result, rendered) = mathExprRender_ e

displayMathCompareSeq :: forall x m . Monad m => Comparisons x (MathExpr x)
                           -> MathematicalLaTeXT m Bool
displayMathCompareSeq comparisons = do
  lift . fromLaTeX . align_ $ inlineFirst renders
  return result
 where inlineFirst (r1:r2:rs) = r1<>r2 : rs
       inlineFirst rs = rs
       ((renders, result), _) = go comparisons
       go :: Comparisons (MathExpr x) -> (([LaTeX], Bool), (x,x))
       go (ExprToCompare x) = (([r],True), (q, q))
        where (q,r) = mathExprRender_ x
       go (ExprComparison c crend l r) = ( ( lrend ++ (((crend"""")& rrendhead) : rrendtail)
                                           , lres && rres && c lrm rlm ()                   )
                                         , ( llm, rrm )                                      )
        where ((lrend              , lres), (llm, lrm)) = go l
              ((rrendhead:rrendtail, rres), (rlm, rrm)) = go r

mathDefinition :: Monad m => MathPrimtvId -> MathExpr b
                                -> MathematicalLaTeXT m(MathExpr b)
mathDefinition varn e = do
   let (val, rendered) = mathExprRender_ e
   lift . fromLaTeX . math $ varn =: rendered
   return $ mathPrimitiv val varn

                               
                               
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

wDefaultTeXMathDisplayConf = (`runReaderT`())




