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
import qualified Text.LaTeX.Packages.AMSMath as AMS

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

askMathHeightsManagement :: MonadReader TeXMathConfiguration m
           => m MathHeightsManagement
askMathHeightsManagement = return ()







-- | This doesn't really belong in this module.
type BracketSize = Int

defaultBracketSize = 0 :: BracketSize

brackSizeCommPrefix :: BracketSize -> Maybe String
brackSizeCommPrefix 0 = Nothing
brackSizeCommPrefix n = Just $ case n of
                              1 -> "big"
                              2 -> "Big"
                              3 -> "bigg"
                              4 -> "Bigg"

latexBrackets :: Maybe BracketSize -> String -> String -> LaTeX -> LaTeX
latexBrackets Nothing ld rd inr = AMS.autoBrackets (raw $ fromString ld) (raw $ fromString rd) inr
latexBrackets (Just sz) ld rd inr = sized 'l' ld <> inr <> sized 'r' rd
 where sized = case brackSizeCommPrefix sz of
        Nothing      -> const $ raw . fromString
        Just prefix  -> \sd brk -> commS $ prefix ++ sd : brk
