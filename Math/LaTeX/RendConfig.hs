-- |
-- Module      : Math.LaTeX.RendConfig
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 

{-# LANGUAGE FlexibleContexts         #-}


module Math.LaTeX.RendConfig where


import Text.LaTeX.Base
import Text.LaTeX.Base.Class

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity

import Data.List
import Data.Function
import Data.String



data MathSymbolTranslations = MathSymbolTranslations
  { defMultiplicationSymbol
  , numeralMultiplicationSymbol
  , atomVarMultiplicationSymbol  :: LaTeX
  }

data MathLaTeXInfixAddenda = MathLaTeXInfixAddenda
  { comparisonLineBreaker :: LaTeX
  }


data TeXMathConfiguration = TeXMathConfiguration
  { mathSymbolTranslations :: MathSymbolTranslations
  , mathLaTeXInfixAddenda :: MathLaTeXInfixAddenda
  }

mathLaTeXExprDefaultConfig :: TeXMathConfiguration
mathLaTeXExprDefaultConfig = TeXMathConfiguration
  { mathSymbolTranslations = MathSymbolTranslations
      { defMultiplicationSymbol     = commS"cdot"
      , numeralMultiplicationSymbol = commS"times"
      , atomVarMultiplicationSymbol = commS","
      }
  , mathLaTeXInfixAddenda = MathLaTeXInfixAddenda
      { comparisonLineBreaker = mempty
      }
  }

askMathSymbolTranslations :: MonadReader TeXMathConfiguration m
           => m MathSymbolTranslations
askMathSymbolTranslations = liftM mathSymbolTranslations ask

askMathLaTeXInfixAddenda :: MonadReader TeXMathConfiguration m
           => m MathLaTeXInfixAddenda
askMathLaTeXInfixAddenda = liftM mathLaTeXInfixAddenda ask
