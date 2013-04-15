-- |
-- Module      : Math.LaTeX.Prelude
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 

module Data.Functor.FixedLength where


data None a = None
instance Functor None where { fmap _ None = None }

data Pair a = Pair a a
instance Functor Pair where { fmap f (Pair l r) = Pair (f l) (f r) }

data Triple a = Triple a a a
instance Functor Triple where { fmap f (Triple l m r) = Triple (f l) (f m) (f r) }


