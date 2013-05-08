{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE RankNTypes                       #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE TypeFamilies                     #-}


import Math.LaTeX.Prelude
import Math.LaTeX.VoidCalc
import Math.LaTeX.Config
import Math.LaTeX.TextMarkup

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import qualified Text.LaTeX.Packages.AMSMath as AMS

import Data.HList
import Data.Complex(Complex(..))
import Data.Complex.Class

import Prelude hiding((^))
import qualified Prelude



subSection, subSubSection :: (Monad m) => LaTeXT m () -> MathematicalLaTeXT_ m
subSection = fromHaTeX . subsection
subSubSection = fromHaTeX . subsubsection


main :: IO ()
main = execLaTeXT simple >>= renderFile "shorttest0.tex"

simple :: Monad m => LaTeXT_ m
simple = do
   thePreamble
   document theBody

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
   documentclass [] article
   usepackage [utf8] inputenc
   usepackage [] AMS.amsmath
   author "Justus Sagemüller"
   title "Simple example"

theBody :: Monad m => LaTeXT_ m
theBody = do
   maketitle
   section "Hello"
   "This is a simple example using the " <> hatex <> " library and some math stuff. "
   
   toHaTeX_wConfig (mathLaTeXDefaultConfig{textMarkupConfig = inlineMarkdown})
           theContents


theContents :: Monad m => MathematicalLaTeXT_ m
theContents = do
 subSection "Arithmetics with infix operators" >> do
   
   n::Integer <- displayMathExpr_ $
             4 ^ (2 ^ 3) ^ 2 - 10000 Prelude.^ 7
   " is "?~? n >> ". "
   
   _::Double <- do
         "For "
         x <- mathDefinition "x" 19
         " and "
         tau <- mathDefinition AMS.tau $ 2*pi
         ", "...:"."
         displayMathExpr_wRResult $
                    2 + 7*(6 - tau) - exp(5 - sqrt(x**2 + 4/pi))
   
   _::[Complex Double] <- mapM displayMathExpr_wRResult [
          2 +| (-1)
        , - exp (log 2 + imagUnit*pi/4)
        ]
   
  
   nl
   
 subSection "Simple finite sums / products" >> do
   
   sums::[Double] <- mapM displayMathExpr_wRResult
      [ lSetSum "n" (listAsFinSet[0,1,4,5]) (2.5 - )
      , limsSum "n" 1 4  (2.5 - )
      , limsSum "j" 1 40 $ cos . (2*pi/40*)
      , realPart (limsSum "j" 1 40 $ cis . (2*pi/40*)
                    :: MathExpr (Complex Double) )
      , 2 * limsSum "i" 1 6 (\i -> i^2 + i) 
      , limsSum "i" 1 6 (\i -> i^2 + i) * 2
      , limsSum "i" 1 6 (\i -> i^2 + i * 2)
      , limsSum "i" 1 6 $ limsSum "j" 1 6 . (*)
      , polyLimsSum "i" 1 6 $ \i -> i * limsSum "j" 1 6 id
      , polyLimsSum "i" 1 6 $ \i -> limsSum "j" 1 i (i*)
      , polyLimsSum "i" 1 6 $ \i -> limsProd "j" 1 i (i*)
      ]
   
   "Sums may also be only well-defined in an analytical sense, e.g. range to infinity, like"...:"."
   displayRealExpr .
       limsSum "j" 1 infty $ (1/) . (^2)
   
   nl
   
 subSection "Function definitions" >> do
   _::Integer <- do
         "For "
         f :: forall a. BasedUpon HNil a => MathLaTeXEval (Integer->Integer) a
           <- mathFuncDefinition "f" "x" $ \x -> x^2 - x
         ", we obtain"...:"."
         displayMathExpr_wRResult $
                (f $$$ 5) + (f $$$ 5^2) - (f $$$ 5)^2
 
   _::Integer <- do
         "For "
         f :: forall a. BasedUpon HNil a => MathLaTeXEval (Integer->Integer) a 
          <- mathFuncDefinition "f" "x" $ \x -> limsSum "n" 1 8 (x-)
         ", "...:"."
         displayMathExpr_wRResult $
                limsSum "x" 1 8 (f $$$)
   nl
 
 subSection "Free variables"; (freeVarIntro "x" :: NewFreeVar Double) $ \x -> do
   "We have now a free variable, ">>inlineMathExpr x>>", in scope. General statements can be made, involving this variable:"...:"."
   displayMathCompareSeq $ forceParens(sin x)^2 + forceParens(cos x)^2 =. 1
   
   (freeVarIntro "f" :: NewFreeVar (Double->Integer)) $ \f -> do
     "Free variables may also have a function type. You can again make statements about those, in the obvious ways:"...:","
     displayMathCompareSeq $ (f $$$ x) =. (f $$$ x + pi) - 5
     "this example being obviously _not_ a correct identity for general functions. At the moment, statements involving free variables can not be checked in any way."
 
 subSection "Checking some simple identities" >> do
   
   testJudge =<< do
      zero <- displayMathExpr_ (
        asin.sin . acos.cos . atan.tan $ 0 )
      return(zero::Double)
      " is "
      inlineRoughValue zero
      ", "
      nonzero <- displayMathExpr_ (
         asinh.sinh . acosh.(/2).cosh . atanh.tanh  $ 0 )
      case nonzero of
        0 -> "as well. "
        _ -> fromHaTeX $ textbf " is not. "
      
      return $ zero==0 && nonzero/=0
   nl
   
   "A simple equations chain:"...:"."
   testJudge =<< do
      displayMathCompareSeq_ $
             10 ^ 18
          =& 10^9 * 10^9
          =& 10^(3^2) * 10^5 * 10^ 4
          =. (1000000000000000000 :: MathExpr Integer)
   nl
   
   "Another equations chain, this time using floats:"...:"."
   testJudge =<< do
       displayMathCompareSeq_ $
                 10 ^ (-18)
              =& 10^(-9) * 10^(-9)
              =& 10^(-3^2) * 10^(-5) * 10^(-4)
              =. (1/1000000000000000000 :: MathExpr Double)
   nl
   
   "Equation-chains can also be approximate (``rough''):"...:"."
   testJudge =<< do
       displayMathCompareSeq_ $
                 10 ^ (-18)
              =~& 10^(-9) * 10^(-9)
              =~& 10^(-3^2) * 10^(-5) * 10^(-4)
              =~. (1/999998765432100000 :: MathExpr Double)
   nl
   
   "Complex exponential identities."
   " (Writing only rough-equalities to avoid problems with the floating-point comparisons, as at the moment only `double` can be used as the underlying data type for the complex arithmetics.)"
   testJudge =<< do
       displayMathCompareSeq_ $
                  exp (3/4 * imagUnit * pi)
              =~& exp (11/4 *imagUnit * pi)
              =~. (-sqrt 2 +| sqrt 2   :: MathExpr (Complex Double)
                                   )/2
   
   
 subSection "Layout tweaking" >> do
  subSubSection "Brackets in math" >> do
   
   let exaDisp = displayMathExpr_wRResultAsTypeOf (0::Double)
   
   "Parentheses are by default set if and only if necessary for the shown expression to match the Haskell source:"
   exaDisp $ 5 - 4 - 3 + 2 + 1
   "contains no parens, nor does the Haskell source. In contrast,"
   exaDisp $ 5 - (4 - 3) + 2 + 1
   "contains parens, which are obligatory since the result is different (subtraction not associative). They can neverless be omitted, but it's of course usually a bad idea -- which is why the needed function is called `unsafeOmitParens`:"...:"???"
   exaDisp $ 5 - unsafeOmitParens(4 - 3) + 2 + 1
   "On the other hand,"
   exaDisp $ 5 - 4 -  3 + (2 + 1)
   "has parens around ">>inlineMathExpr_(2+1)>>" _in the source_, but they aren't relevant -- addition _is_ associative -- and can thus be omitted in the output. Then again,"
   exaDisp $ 5 - 4 -  3 + forceParens(2+1)
   "wouldn't need the parens either, but they're enforced via the `forceParens` function (which is perfectly safe)."
   nl
   "By default, delimiters are normally scaled to suitable size (by calling ">>fromHaTeX latex>>"'s `\\left(` and `\\right)` macros) if necessary:"
   exaDisp $ ((5+2)/8 + 1) * 4 
   "The behaviour can be tweaked with the `manBracketSize` function, like"...:","
   exaDisp $ manBracketSize 0 ((5+2)/8 + 1) * 4 
   "or"...:"."
   exaDisp $ manBracketSize 3 (5+1) * 2
   nl
  
  subSubSection "Text markup" >> do
   "It can be tedious to switch into a completely different mode every time you need to do a non-plaintext operation, like simply emphasising ">>fromHaTeX(textit"single words")>>". To avoid this, we offer customisable markup on literal text passages. Deliberately avoiding any ``proper'' fixed syntax like ">>fromHaTeX latex>>"'s questionably mature one, this is based on general regex matching."
   nl
   "A popular markup language is *Markdown*, which is lightweight and allows simple stuff like _italic_, **bold** and `verbatim` with very little overhead. Upon such a simple basis, you can "
   addMarkupRule"#([^#][^#]*)#"(textsc::LaTeX->LaTeX)
    .addMarkupRule"\\^([^^][^^]*)\\^"(textsf::LaTeX->LaTeX) $ do
    "at ^any^ point add extra #rules#, "
    addMarkupRule"([aeiou])"((raw::Text->LaTeX).("\\\""<>)) $ do
     "to arbitrarily "
     addMarkupRule" "("_"::LaTeX)
      .addMarkupRule"([rcsx])"(textbf . raw::Text->LaTeX)
      .addMarkupRule"([dt])"(textit . raw::Text->LaTeX) $ do
      "ridiculous extend. "
   "Since all configuration changes are safely encapsulated in a reader monad, there is no way they can somehow leak into a global state, you _always_ get back to the correct scope's configuration." >> nl
   "Text markup is really just meant for simple, frequent stuff that occurs in plain text, like emphasis, accents or single special characters. For anything more complicated, it's better to operate openly in the rendering monad, giving you the blessings of Haskell's type safety."
   
   
   

testJudge :: Monad m => Bool -> MathematicalLaTeXT_ m
testJudge True = "(Test passed.)"
testJudge _    = "(" <> failMsg <> ".)" where
 failMsg = fromHaTeX $ do textbf "Test failed"
                          footnote "Even true mathematical identities may not show to hold when using floating-point arithmetics."

