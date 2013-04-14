{-# LANGUAGE OverloadedStrings                #-}


import Math.LaTeX.Prelude

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.AMSMath

-- Document structure copied from Daniel Diaz' 'Examples/simple.hs'.


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
   usepackage [] amsmath
   author "Justus Sagemüller"
   title "Simple example"

theBody :: Monad m => LaTeXT_ m
theBody = do
   maketitle
   section "Hello"
   "This is a simple example using the " <> hatex <> " library and some math stuff. "
   
   n <- mathTestInteger
   " is " <> rendertex n <> "."
   
   newline
   x <- mathTestFloating
   return(x::Double)
   " is approximately "
   wDefaultTeXMathDisplayConf $ inlineRoughValue x
   ". "
   
   newline
   mathFloatSumTest
   
   newline
   
   mathTestTrigInverses >>= testResult
   newline
   
   "A simple equations chain:"
   mathTestIntegerEquationchain >>= testResult
   newline
   
   "Another equations chain, this time using floats:"
   mathTestFloatEquationchain >>= testResult
   
   
   
   
mathTestFloating :: (Monad m, Floating n, Show n) => LaTeXT m n
mathTestFloating = wDefaultTeXMathDisplayConf $ do
   "For "
   x <- mathDefinition "x" 19
   " and "
   τ <- mathDefinition tau $ 2*pi
   ", "
   displayMathExpr_ $
              2 + 7*(6 - τ) - exp(5 - sqrt(x**2 + 4/pi))
              
mathTestInteger :: Monad m => LaTeXT m Integer
mathTestInteger = wDefaultTeXMathDisplayConf $ do
   displayMathExpr_ $
             4 ^* (2 ^* 3) ^* 2 - 10000^7

mathTestTrigInverses :: Monad m => LaTeXT m Bool
mathTestTrigInverses = wDefaultTeXMathDisplayConf $ do
   zero <- displayMathExpr_ (
     asin.sin . acos.cos . atan.tan $ 0 )
   return(zero::Double)
   " is "
   inlineRoughValue zero
   ", "
   nonzero <- displayMathExpr_ (
      asinh.sinh . acosh.(/2).cosh . atanh.tanh  $ 0 )
   case nonzero of
     0 -> "as well."
     _ -> lift $ textbf " is not."
   
   return $ zero==0 && nonzero/=0

mathTestIntegerEquationchain :: Monad m => LaTeXT m Bool
mathTestIntegerEquationchain = wDefaultTeXMathDisplayConf $ do
   displayMathCompareSeq_ $
        10 ^* 18
     =& 10^*9 * 10^*9
     =& 10^*(3^*2) * 10^*5 * 10^* 4
     =. (1000000000000000000 :: MathExpr Integer)

mathTestFloatEquationchain :: Monad m => LaTeXT m Bool
mathTestFloatEquationchain = wDefaultTeXMathDisplayConf $ do
   let compareChain =
             10 ^* (-18)
          =& 10^*(-9) * 10^*(-9)
          =& 10^*(-3^*2) * 10^*(-5) * 10^*(-4)
          =. (1/1000000000000000000 :: MathExpr Double)
   displayMathCompareSeq_ compareChain
   
mathFloatSumTest :: Monad m => LaTeXT m [Double]
mathFloatSumTest = wDefaultTeXMathDisplayConf $
   mapM displayMathExpr_wRResult
      [ lSetSum "n" (listAsFinSet[1,2,3,4]) (2.5 - )
      , finRSum "n" 1 4  (2.5 - )
      , finRSum "j" 1 40 $ cos . (2*pi/40*)
      , 2 * finRSum "i" 1 6 (\i -> i^*2 + i) 
      , finRSum "i" 1 6 (\i -> i^*2 + i) * 2
      , finRSum "i" 1 6 (\i -> i^*2 + i) + 2
      , finRSum "i" 1 6 (\i -> i^*2 + i + 2)
      ]


testResult :: Monad m => Bool -> LaTeXT_ m
testResult True = "(Test passed.)"
testResult _    = textbf "(Test failed.)"
