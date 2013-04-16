-- |
-- Module      : Data.HList.BasedUpon
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

-- Ugly manually-scripted instances, to avoid OverlappingInstances trouble.
-- Should of course be done, if at all, with TemplateHaskell.
-- @
-- mapM_ putStrLn ["instance BasedUpon fund "++(conses >>= \c -> "(HCons "++c:" ")++"fund"++map(const ')')conses++"where\n  basement "++(conses >> "(HCons _ ")++"f"++map(const ')')conses++" = f" | conses<-[['a'..x]|x<-['a'..'h']]]
-- @
instance BasedUpon fund (HCons a fund)where
  basement (HCons _ f) = f
instance BasedUpon fund (HCons a (HCons b fund))where
  basement (HCons _ (HCons _ f)) = f
instance BasedUpon fund (HCons a (HCons b (HCons c fund)))where
  basement (HCons _ (HCons _ (HCons _ f))) = f
instance BasedUpon fund (HCons a (HCons b (HCons c (HCons d fund))))where
  basement (HCons _ (HCons _ (HCons _ (HCons _ f)))) = f
instance BasedUpon fund (HCons a (HCons b (HCons c (HCons d (HCons e fund)))))where
  basement (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ f))))) = f
instance BasedUpon fund (HCons a (HCons b (HCons c (HCons d (HCons e (HCons f fund))))))where
  basement (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ f)))))) = f
instance BasedUpon fund (HCons a (HCons b (HCons c (HCons d (HCons e (HCons f (HCons g fund)))))))where
  basement (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ f))))))) = f
instance BasedUpon fund (HCons a (HCons b (HCons c (HCons d (HCons e (HCons f (HCons g (HCons h fund))))))))where
  basement (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ f)))))))) = f