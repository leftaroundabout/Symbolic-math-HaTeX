-- |
-- Module      : Math.LaTeX.StringLiterals
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : requires GHC>7 extensions
-- 
-- An orphan instance to the 'FromString' class, which allows maths expressions
-- to include literal strings if you enable @{-# LANGUAGE OverloadedStrings #-}@.
-- These will be rendered in roman font.

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.LaTeX.StringLiterals where

import qualified Text.LaTeX as LaTeX
import Text.LaTeX.Base.Syntax (LaTeX)
import qualified Text.LaTeX.Packages.AMSMath as LaTeX

import CAS.Dumb
import CAS.Dumb.Tree
import CAS.Dumb.Symbols

import Data.String


instance (SymbolClass σ, SCConstraint σ LaTeX)
     => IsString (CAS' γ (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)) where
  fromString = Symbol . StringSymbol . LaTeX.mathrm . fromString
