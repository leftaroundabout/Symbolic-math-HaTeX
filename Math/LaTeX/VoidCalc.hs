-- |
-- Module      : Math.LaTeX.VoidCalc
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 
{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE GADTs                            #-}
{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE UndecidableInstances             #-}
-- {-# LANGUAGE OverlappingInstances             #-}
{-# LANGUAGE PatternGuards                    #-}
{-# LANGUAGE TypeFamilies                     #-}


module Math.LaTeX.VoidCalc where


import Math.LaTeX.Prelude


import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Void

import Data.List
import Data.Function
import Data.Ratio
import Data.Functor.Contravariant
import Data.String



-- | Type for mathematical expressions that cannot be calculated
-- by this library, but can still be typeset via LaTeX.
type MathHard a = MathLaTeXEval a Void

