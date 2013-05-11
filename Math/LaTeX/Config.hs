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
  { tupleSeperatorSymbol :: LaTeX
  
  , defMultiplicationSymbol
  , numeralMultiplicationSymbol
  , atomVarMultiplicationSymbol :: LaTeX
  
  , linmapComposeMultiplicationSymbol
  , numeralLinmapMultiplicationSymbol
  , atomLinmapComposeMultiplicationSymbol :: LaTeX
  , functionApplySymb
  , numeralfunctionApplySymb        -- ^ A numeral-function is basically a number, interpreted as @μ⋅id@
  , functionToNumeralApplySymb
  , functionToAtomApplySymb
--   , functionToKetApplySymb
     :: Maybe LaTeX
     -- ^ When 'Nothing', no symbol is used but the common writing style \"&#x1d453;(&#x1d465;)\" employed (further specified through the @forceParensAround*@ fields).
  
  , functionApplySpacingSymb :: LaTeX
     -- ^ The spacing (or symbol) to use when none of the @*function*ApplySymb@s apply, but no parens around the arguments are enforced.
      
  , forceParensAroundAtomFunctionArgs  -- True -> f(x), False -> f x
      -- ^ @True@ &#x2192; &#x1d453;(&#x1d465;), @False@ &#x2192 &#x1d453; &#x1d465;
  , forceParensAroundNumFunctionArgs   -- True -> f(24), False -> f 24
      -- ^ @True@ &#x2192; &#x1d453;(24), @False@ &#x2192; &#x1d453; 24
  , forceParensAroundAtomArgsToStdFunc -- True -> sin(x), False -> sin x
      -- ^ @True@ &#x2192; sin(&#x1d465;), @False@ &#x2192; sin &#x1d465;
  , forceParensAroundNumArgsToStdFunc :: Bool
      -- ^ @True@ &#x2192; sin(2.4), @False@ &#x2192 sin 2.4
      
  , lambdafunctionVisualisation  :: LaTeX -> LaTeX -> LaTeX
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
      { tupleSeperatorSymbol = fromString","
       
      , defMultiplicationSymbol     = commS"cdot"
      , numeralMultiplicationSymbol = commS"times"
      , atomVarMultiplicationSymbol = commS","
      
      , linmapComposeMultiplicationSymbol = commS","
      , numeralLinmapMultiplicationSymbol = commS"cdot"
      , atomLinmapComposeMultiplicationSymbol = commS","
      
      , functionApplySymb = Nothing
      , numeralfunctionApplySymb = Just $ commS"cdot"
      , functionToNumeralApplySymb = Nothing
      , functionToAtomApplySymb = Nothing
--       , functionToKetApplySymb = Just $ commS","
      
      , functionApplySpacingSymb = commS","
      
      , forceParensAroundAtomFunctionArgs = True
      , forceParensAroundNumFunctionArgs = True
      , forceParensAroundAtomArgsToStdFunc = False
      , forceParensAroundNumArgsToStdFunc = True
      
      , lambdafunctionVisualisation = \x f -> x <> commS"mapsto" <> f
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




