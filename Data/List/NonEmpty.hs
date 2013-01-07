-- This should really be a standard import, but the version from NonEmpty is way too thin
-- (not even a functor instance). NonEmptyList, on the other hand, is bloated and includes
-- not just the desired list type but also some Applicative.Zip thing, which doesn't compile
-- with recent GHC versions.

module Data.List.NonEmpty where

import Control.Applicative

infixr 5 :!, ++!

data NonEmpty a = Eventually a
                | a :! NonEmpty a
                deriving(Eq,Show)

(++!) :: NonEmpty a -> NonEmpty a -> NonEmpty a
Eventually x ++! xs = x :! xs
(x:!xs) ++! xs' = x:!(xs++!xs')

neJoin :: NonEmpty(NonEmpty a) -> NonEmpty a
neJoin (Eventually xs) = xs
neJoin (xs :! xss) = xs ++! neJoin xss

instance Functor NonEmpty where
  fmap f(Eventually x) = Eventually $ f x
  fmap f(x:!xs) = f x :! fmap f xs

instance Applicative NonEmpty where
  pure = Eventually
  Eventually f <*> xs = fmap f xs
  f:!fs <*> xs = fmap f xs ++! (fs<*>xs)
  
instance Monad NonEmpty where
  return = Eventually
  xs >>= b  = neJoin $ fmap b xs