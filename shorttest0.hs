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
   " is approximately "
   wDefaultTeXMathDisplayConf . inlineMathExpr $ prettyFloatApprox x
   ". "
   
   inversesCorrect <- mathTestTrigInverses
   if inversesCorrect then "(Test passed.)"
                      else textbf "(Test failed.)"
   
   
   
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
      " is "
      inlineMathExpr(prettyFloatApprox zero)
      ", "
      nonzero <- displayMathExpr_ (
         asinh.sinh . acosh.(/2).cosh . atanh.tanh  $ 0 )
      case nonzero of
        0 -> "as well."
        _ -> lift $ textbf " is not."
      
      return $ zero==0 && nonzero/=0



