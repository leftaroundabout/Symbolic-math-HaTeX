-- |
-- Module      : Data.Complex.Class
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 
-- Haskell98 implements 'Complex' as a simple algebraic data type,
-- consisting of disjoint real- and imaginary-part fields. This
-- is not good for anything beyond plain numeric calculations,
-- particularly not for the symbolic-maths output in 'Math.LaTeX';
-- for instance, @e**(i * φ)@ would invariably render to something
-- Euler-ish @cos φ + i ⋅ sin φ@, which is usually not desirable.
-- This can be fixed by reinterpreting the functions from "Data.Complex"
-- as methods of a more general class. By making the plain
-- old ADT an instance of this, this efficient implementation can
-- still be used most of the time, but doesn't need to.

{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PatternGuards          #-}



module Data.Complex.Class where

import qualified Data.Complex
import Data.Complex(Complex(..))


infixl 6 +|

-- | Minimal complete definition: '(+|)'
-- or both 'realAsComplex' and 'imagAsComplex', and
-- either 'fromComplexC' or both 'realPart' and 'imagPart',
-- and finally 'phase'.
class (Floating c, Floating (RealAxis c)) => ComplexC c where
  type RealAxis c :: *
  
  imagUnit :: c
  imagUnit = 0+|1
  
  -- | @a +| b ≡ a + i⋅b@, i.e. build a complex number out of
  -- real and imaginary part.
  -- 
  -- @
  -- infixl 6 +|
  -- @
  (+|) :: RealAxis c -> RealAxis c -> c
  a +| b = realAsComplex a + imagAsComplex b
  
  conjugate :: c->c
  conjugate c | (a:+b)<-fromComplexC c
                   = a +| negate b
  
  fromComplexC :: c -> Complex (RealAxis c)
  fromComplexC c = realPart c :+ imagPart c
  
  toComplexC :: Complex (RealAxis c) -> c
  toComplexC (a:+b) = a +| b
  
  realAsComplex, imagAsComplex :: RealAxis c -> c
  realAsComplex a = a +| 0
  imagAsComplex b = 0 +| b
  
  realPart, imagPart :: c ->RealAxis c 
  realPart = (\(a:+_)->a) . fromComplexC
  imagPart = (\(_:+b)->b) . fromComplexC
  
  
  magnitude, phase :: c -> RealAxis c
  magnitude = (\(a:+b) -> sqrt(a*a + b*b) ) . fromComplexC
  
  mkPolar :: RealAxis c -> RealAxis c -> c
  mkPolar r = (realAsComplex r *) . cis
  
  cis :: RealAxis c -> c
  cis = exp . imagAsComplex
  
  polar :: c -> (RealAxis c, RealAxis c)
  polar z = (magnitude z, phase z)
  

instance (RealFloat r) => ComplexC (Complex r) where
  type RealAxis (Complex r) = r
  
  imagUnit = 0:+1
  (+|) = (:+)
  fromComplexC = id
  toComplexC = id
  conjugate = Data.Complex.conjugate
  realPart  = Data.Complex.realPart
  imagPart  = Data.Complex.imagPart
  
  cis = Data.Complex.cis
  magnitude = Data.Complex.magnitude
  phase = Data.Complex.phase
  mkPolar = Data.Complex.mkPolar
  polar = Data.Complex.polar
  
  
  
  