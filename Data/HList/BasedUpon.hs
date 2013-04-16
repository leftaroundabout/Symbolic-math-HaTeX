-- |
-- Module      : Math.LaTeX.VoidCalc
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 

{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE FlexibleInstances                #-}

module Data.HList.BasedUpon where



import Data.HList



class BasedUpon fund house where
  basement :: house -> fund


instance BasedUpon fund fund where
  basement = id

instance (BasedUpon fund low) => BasedUpon fund (HCons a low) where
  basement (HCons a f) = basement f