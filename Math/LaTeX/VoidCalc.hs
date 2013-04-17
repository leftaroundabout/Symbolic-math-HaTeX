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
{-# LANGUAGE OverlappingInstances             #-}
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
import Data.HList
import Data.Function
import Data.Functor.FixedLength
import Data.Ratio
import Data.Functor.Contravariant
import Data.String



-- | Base type for mathematical expressions that cannot be calculated
-- by this library, but can still be typeset via LaTeX. Note that
-- the expression's result type is still well-defined, the 'Void'
-- represents the unobtainable arguments that would be required
-- to calculate it.
type MathHard a = MathLaTeXEval a Void


class IsVoid v where
  absurdV :: v -> a
  absurdV l = l `seq` undefined
-- Overlapping instances are safe here since all possible implementations
-- of this function are equivalent anyway, provided the type does
-- indeed not have any constructors.

instance IsVoid Void where
  absurdV = absurd

instance IsVoid (HCons Void l)

instance (IsVoid l) => IsVoid (HCons e l)


-- | A variable nothing is known about, safe for its supposed return type.
unknown :: IsVoid arg => MathPrimtvId -> MathLaTeXEval r arg
unknown = mathDepPrimitiv absurdV

