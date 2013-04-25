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


type TeXMathConfiguration = MathSymbolTranslations

mathLaTeXExprDefaultConfig :: TeXMathConfiguration
mathLaTeXExprDefaultConfig = MathSymbolTranslations
      { defMultiplicationSymbol     = commS"cdot"
      , numeralMultiplicationSymbol = commS"times"
      , atomVarMultiplicationSymbol = commS","
      }


askMathSymbolTranslations :: MonadReader TeXMathConfiguration m
           => m MathSymbolTranslations
askMathSymbolTranslations = ask
