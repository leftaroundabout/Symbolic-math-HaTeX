{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE GADTs                            #-}
{-# LANGUAGE ConstraintKinds                  #-}
{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE MultiParamTypeClasses            #-}

import Text.LaTeX.Base
import Text.LaTeX.Packages.Inputenc
-- import qualified Text.LaTeX.Packages.AMSMath as HTXM
import qualified Data.Text as T

import Control.Monad.Reader
import Control.Monad.Identity

import Data.Function
import Data.Ratio
import Data.Functor.Contravariant



-- Document structure copied from Daniel Diaz' 'Examples/simple.hs'.


main :: IO ()
main = execLaTeXT simple >>= renderFile "shorttest0.tex"

simple :: Monad m => LaTeXT_ m
simple = do
   thePreamble
   document theBody

mathTestFloating :: (Monad m, Floating n, Show n) => LaTeXT m n
mathTestFloating = wDefaultTeXMathDisplayConf $ do
   lift "For "
   x <- mathDefinition "x" 19
   lift " and "
   tau <- mathDefinition "\\tau" $ 2*pi
   lift ", "
   displayMathExpr $
              2 + 7*(6 - tau) - exp(5 - sqrt(x**2 + 4/pi))
              
mathTestInteger :: Monad m => LaTeXT m Integer
mathTestInteger = wDefaultTeXMathDisplayConf $ do
   displayMathExpr $
             4 ^: (2 ^: 3) ^: 2 - 10000^7

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
   documentclass [] article
   usepackage [utf8] inputenc
   author "Justus Sagemüller"
   title "Simple example"

theBody :: Monad m => LaTeXT_ m
theBody = do
   maketitle
   section "Hello"
   "This is a simple example using the "
   hatex
   " library and some math stuff. "
   
   n <- mathTestInteger
   " is "
   rendertex n
   ". "
   
   newline
   x <- mathTestFloating
   " is approximately "
   wDefaultTeXMathDisplayConf . inlineMathExpr $ prettyFloatApproxExprn x
   ". "











-- Some trivial fixed-size-array functors, these model the structural recursion of
-- either primitive math (None), unary functions (Identity), or infix functions (Pair).
data None a = None
instance Functor None where { fmap _ None = None }
data Pair a = Pair a a
instance Functor Pair where { fmap f (Pair l r) = Pair (f l) (f r) }




type MathPrimtvId = Text
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
           , enclosingLaTeX :: list Text -> Text
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

type MathExpr a = MathLaTeXEval a ()


mathExprRender :: forall b. MathLaTeXEval b () -> (b, Text)
mathExprRender (MathLaTeXEval e _) = (calculated e (), rendered e)
 where calculated :: MathEvaluation d c -> c -> d
       calculated (MathEnvd f arg _ enclosed) c
            = f (fmap(\(MathLaTeXEval e' _) a
                               -> calculated e' (a,c) ) $ enclosed arg) c
--        rendered (MathPrimitive _ txm) = txm
--        rendered (MathDepdPrimitive _ txf) = txf i
       rendered :: MathEvaluation c a -> Text
       rendered (MathEnvd _ a txf enclosed)
            = txf . fmap(rendered . mathLaTeXevaluation) $ enclosed a

mathPrimtv :: b -> Text -> MathLaTeXEval b a
-- mathPrimtv v name = MathLaTeXEval (MathPrimitive v name) 10
mathPrimtv v name
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
         | isotropFixity fxty <= 9   = T.concat [ fn, "{\\left(", incl, "\\right)}" ]
         | otherwise                 = T.concat [ fn, "\\:{", incl, "}" ]

 

mathExprInfix :: (a->a->r)
                 -> (MathPrimtvId -> MathPrimtvId -> MathPrimtvId)
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
 where ifxNamer lexpr rexpr = T.concat
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
       plain expr = T.concat ["{", expr, "}"]
       parenthd expr = T.concat ["{\\left(", expr, "\\right)}"]
       

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
  fromInteger n = mathPrimtv (fromInteger n)  (render n)
  
  (+) = mathExprIfx (+) "+" $ Infixl 6
  (-) = mathExprIfx (-) "-" $ Infixl 6
  (*) = mathExprIfx (*) "\\cdot" $ Infixl 7
  
  signum = mathExprFn abs "\\mathrm{sgn}"
  abs = (`MathLaTeXEval`Infix 9) . mathExprFunction abs
           (\e -> T.concat ["\\left|", e, "\\right|"] )


infixr 8 ^:
class Num x => Powerable x where
  (^:) :: x -> x -> x
instance Powerable Int where { (^:) = (^) }
instance Powerable Double where { (^:) = (**) }
instance Powerable Float where { (^:) = (**) }
instance Powerable Integer where { (^:) = (^) }
instance (Powerable res, Show res) => Powerable (MathLaTeXEval res arg) where
  (^:) = mathExprIfx (^:) "^" $ Infixr 8


instance (Fractional res, Show res) => Fractional (MathLaTeXEval res arg) where
  fromRational e = (`MathLaTeXEval`Infix 9) $ mathExprInfix (/)
           (\n d -> T.concat ["\\tfrac{", n, "}{", d, "}"] )
           (fromIntegral $ numerator e) (fromIntegral $ denominator e)
  
  a/b = (`MathLaTeXEval`Infix 9) $ mathExprInfix (/)
           (\n d -> T.concat ["\\frac{", n, "}{", d, "}"] ) a b
  
  recip = (`MathLaTeXEval`Infix 9) . mathExprFunction recip
           (\e -> T.concat ["\\frac1{", e, "}"] )

instance (Floating res, Show res) => Floating (MathLaTeXEval res arg) where
  pi = mathPrimtv pi "\\pi"
  
  sqrt = (`MathLaTeXEval`Infix 9) . mathExprFunction sqrt
           (\x -> T.concat ["\\sqrt{", x, "}"] )
           
  exp = (`MathLaTeXEval`Infix 8) . mathExprFunction exp
           (\x -> T.concat ["e^{", x, "}"] )
--   b**x = (`MathLaTeXEval`Infixr 8) $ mathExprInfix (**)
  (**) = mathExprIfx (**) "^" $ Infixr 8
--            (\β ξ -> T.concat [ "{", β, "}^{", ξ, "}"] ) b x
           
  log = mathExprFn log "\\ln"
  logBase b t = (`MathLaTeXEval`Infix 9) $ mathExprInfix logBase
           (\β τ -> T.concat ["\\log_{", β, "}\\left(", τ, "\\right"] ) b t
  
  sin = mathExprFn sin "\\sin"
  cos = mathExprFn cos "\\cos"
  tan = mathExprFn tan "\\tan"
  asin = mathExprFn asin "\\arcsin"
  acos = mathExprFn acos "\\arccos"
  atan = mathExprFn atan "\\arctan"
  sinh = mathExprFn sinh "\\sinh"
  cosh = mathExprFn cosh "\\cosh"
  tanh = mathExprFn tanh "\\tanh"
  asinh = mathExprFn asinh "\\arcsinh"
  acosh = mathExprFn acosh "\\arccosh"
  atanh = mathExprFn atanh "\\arctanh"
  

prettyFloatApproxExprn :: Double -> MathExpr Double
prettyFloatApproxExprn x
    | (mantissa, e:expon) <- break(=='e') s
    , m<-read $ take 7 mantissa, expn<-read expon
                = prettyFloatApproxExprn m * 10 ^: fromInteger expn
    | otherwise = mathPrimtv x $ T.pack s
 where s = show x
  



inlineMathExpr :: Monad m => MathExpr b -> MathematicalLaTeXT m b
inlineMathExpr e = do
   lift . raw $ T.concat ["$", rendered, "$ "]
   return result
 where (result, rendered) = mathExprRender e

displayMathExpr :: Monad m => MathExpr b -> MathematicalLaTeXT m b
displayMathExpr e = do
   lift . raw $ T.concat ["\n\\[ ", rendered, " \\]\n"]
   return result
 where (result, rendered) = mathExprRender e

mathDefinition :: Monad m => MathPrimtvId -> MathExpr b
                                -> MathematicalLaTeXT m(MathExpr b)
mathDefinition varn e = do
   let (val, rendered) = mathExprRender e
   lift . raw $ T.concat ["$", varn, " = ", rendered, "$"]
   return $ mathPrimtv val varn

-- data MathExpr numConstraint numResult :: * where
--   MathPrimitive { mathPrimIdtyfier :: MathPrimtvId
--                 , mathPrimValue  } :: MathPrimtvId -> MathExpr cstr a
--   MathEnvd { enclosingFunction :: a -> b
--            , enclosingLaTeX
--            , enclosedMathExpr :: MathExpr a } :: cstr a, cstr b => MathExpr b


data TeXMathExpr = TeXMathExpr { mathExprRendered :: Text
                               , mathExpr_isSimple :: Bool
                               , mathExpr_fixity :: Int 
                               }
                               
                               
          -- TeXMathDisplayConf should eventually contains things
          -- like the default way to render e.g. multiplication
          -- ('\cdot' vs '\times' or what environments to use.
type TeXMathDisplayConf = ()


type MathematicalLaTeXT m a = ReaderT TeXMathDisplayConf (LaTeXT m) a
type MathematicalLaTeXT_ m = MathematicalLaTeXT m ()  -- ReaderT TeXMathDisplayConf (LaTeXT m) ()
type MathematicalLaTeX a = MathematicalLaTeXT Identity a  -- ReaderT TeXMathDisplayConf (LaTeXT Identity) a
type MathematicalLaTeX_ = MathematicalLaTeXT Identity () -- ReaderT TeXMathDisplayConf (LaTeXT Identity) ()

wDefaultTeXMathDisplayConf = (`runReaderT`())


-- inlineTeXMath :: TeXMathExpr -> MathematicalLaTeX_
-- inlineTeXMath (TeXMathExpr rendd _ _) = lift $ raw rendd

-- mathPrimtv :: Text -> TeXMathExpr
-- mathPrimtv name = TeXMathExpr name True 9

-- instance Num TeXMathExpr where
--   fromInteger = mathPrimtv . render
--   TeXMathExpr 


