-- |
-- Module      : Math.LaTeX.Config
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 

{-# LANGUAGE FlexibleContexts         #-}


module Math.LaTeX.Config where


import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import qualified Text.LaTeX.Packages.AMSMath as AMS

import Math.LaTeX.TextMarkup
import Math.LaTeX.Internal.Misc.BracketSizes

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity

import Data.List
import Data.Function
import Data.String
import qualified Data.Text as T



data MathSymbolTranslations = MathSymbolTranslations
  { defMultiplicationSymbol
  , numeralMultiplicationSymbol
  , atomVarMultiplicationSymbol  :: LaTeX
  }

data MathLaTeXInfixAddenda = MathLaTeXInfixAddenda
  { comparisonLineBreaker :: LaTeX
  }
  


type MathHeightsManagement = ()



data TeXMathConfiguration = TeXMathConfiguration
  { mathSymbolTranslations :: MathSymbolTranslations
  , mathLaTeXInfixAddenda :: MathLaTeXInfixAddenda
  , textMarkupConfig :: TextMarkupConfig
  }

mathLaTeXDefaultConfig :: TeXMathConfiguration
mathLaTeXDefaultConfig = TeXMathConfiguration
  { mathSymbolTranslations = MathSymbolTranslations
      { defMultiplicationSymbol     = commS"cdot"
      , numeralMultiplicationSymbol = commS"times"
      , atomVarMultiplicationSymbol = commS","
      }
  , mathLaTeXInfixAddenda = MathLaTeXInfixAddenda
      { comparisonLineBreaker = mempty
      }
  , textMarkupConfig = noTextMarkup
  }

askMathSymbolTranslations :: MonadReader TeXMathConfiguration m
           => m MathSymbolTranslations
askMathSymbolTranslations = liftM mathSymbolTranslations ask

askMathLaTeXInfixAddenda :: MonadReader TeXMathConfiguration m
           => m MathLaTeXInfixAddenda
askMathLaTeXInfixAddenda = liftM mathLaTeXInfixAddenda ask

askMathHeightsManagement :: MonadReader TeXMathConfiguration m
           => m MathHeightsManagement
askMathHeightsManagement = return ()

askTextMarkupConfig :: MonadReader TeXMathConfiguration m
           => m TextMarkupConfig
askTextMarkupConfig = liftM textMarkupConfig ask

instance HasTextMarkupConfig TeXMathConfiguration where
  modifyMarkupRules f cfg = cfg { textMarkupConfig = modifyMarkupRules f $ textMarkupConfig cfg }




