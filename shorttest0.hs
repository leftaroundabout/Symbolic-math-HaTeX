{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE ScopedTypeVariables              #-}


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
   
   wDefaultConf_toHaTeX theContents


theContents :: Monad m => MathematicalLaTeXT_ m
theContents = do
   
   fromHaTeX $ subsection "Arithmetics with infix operators"
   
   n::Integer <- displayMathExpr_ $
             4 ^* (2 ^* 3) ^* 2 - 10000^7
   " is "?~? n >> ". "
   
   _::Double <- do
         "For "
         x <- mathDefinition "x" 19
         " and "
         τ <- mathDefinition tau $ 2*pi
         ", "...:"."
         displayMathExpr_wRResult $
                    2 + 7*(6 - τ) - exp(5 - sqrt(x**2 + 4/pi))
   
   fromHaTeX $ subsection "Simple finite sums"
   
   sums::[Double] <- mapM displayMathExpr_wRResult
      [ lSetSum "n" (listAsFinSet[0,1,4,5]) (2.5 - )
      , finRSum "n" 1 4  (2.5 - )
      , finRSum "j" 1 40 $ cos . (2*pi/40*)
      , 2 * finRSum "i" 1 6 (\i -> i^*2 + i) 
      , finRSum "i" 1 6 (\i -> i^*2 + i) * 2
      , finRSum "i" 1 6 (\i -> i^*2 + i) + 2
      ]
   
   fromHaTeX $ subsection "Checking some simple identities"
   
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
             10 ^* 18
          =& 10^*9 * 10^*9
          =& 10^*(3^*2) * 10^*5 * 10^* 4
          =. (1000000000000000000 :: MathExpr Integer)

   nl
   
   "Another equations chain, this time using floats:"...:"."
   testJudge =<< do
       let compareChain =
                 10 ^* (-18)
              =& 10^*(-9) * 10^*(-9)
              =& 10^*(-3^*2) * 10^*(-5) * 10^*(-4)
              =. (1/1000000000000000000 :: MathExpr Double)
       displayMathCompareSeq_ compareChain
   
   
   
   

testJudge :: Monad m => Bool -> MathematicalLaTeXT_ m
testJudge True = "(Test passed.)"
testJudge _    = do
    fromHaTeX $ textbf "Test failed. "
    "Even true mathematical identities may not show to hold when using floating-point arithmetics."

