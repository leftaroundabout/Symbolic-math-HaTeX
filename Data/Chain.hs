module Data.Chain where

import Data.Monoid
import Data.Bifunctor
import Data.Bifoldable

data Chain link el = Middle el
                   | Couple el link el
                   | Ends el link (Chain link el) link el
                   deriving(Eq,Show)

fromLists :: [link] -> [el] -> Chain link el -- both finite; @length ls@ should be @length els - 1@.
fromLists lns els = fromLs' flns (reverse blns) els (reverse els)
 where fromLs' (α:llns) (ω:rlns) (a:lels) (z:rels)
          = Ends a α (fromLs' llns rlns lels rels) ω z
       fromLs' _ (mln:_) (lel:_) (rel:_) = Couple lel mln rel
       fromLs' _ _ (mid:_) _ = Middle mid
       (flns,blns) = splitAt(length lns `quot` 2) lns

toLists :: Chain link el -> ([link], [el])
toLists chain = let(lnss,elss) = toLs' chain in (lnss[], elss[])
 where toLs' (Middle el) = (id, (el:))
       toLs' (Couple lel mln rel) = ((mln:), (lel:).(rel:))
       toLs' (Ends a α mid ω z)
         = let (lnss,elss) = toLs' mid in ((α:).lnss.(ω:), (a:).elss.(z:))

leftEnd :: Chain link el -> el
leftEnd (Middle el) = el
leftEnd (Couple el _ _) = el
leftEnd (Ends el _ _ _ _) = el

rightEnd :: Chain link el -> el
rightEnd (Middle el) = el
rightEnd (Couple _ _ el) = el
rightEnd (Ends _ _ _ _ el) = el

chainConsl :: el -> link -> Chain link el -> Chain link el
chainConsl a α (Middle z) = Couple a α z
chainConsl a α (Couple m ω z) = Ends a α (Middle m) ω z
chainConsl a α (Ends b β mid ω z) = Ends a α (chainConsl b β mid) ω z

chainConsr :: Chain link el -> link -> el -> Chain link el
chainConsr (Middle a) ω z = Couple a ω z
chainConsr (Couple a α m) ω z = Ends a α (Middle m) ω z
chainConsr (Ends a α mid ψ y) ω z = Ends a α (chainConsr mid ψ y) ω z

instance Functor(Chain link) where
  fmap f (Middle el) = Middle $ f el
  fmap f (Couple a μ z) = Couple (f a) μ (f z)
  fmap f (Ends a α m ω z) = Ends (f a) α (fmap f m) ω (f z)

linkMap_ :: (α->β) -> Chain α el -> Chain β el
linkMap_ f (Middle el) = Middle el
linkMap_ f (Couple a μ z) = Couple a (f μ) z
linkMap_ f (Ends a α m ω z) = Ends a (f α) (linkMap_ f m) (f ω) z

linkMap :: (el->α->el->β) -> Chain α el -> Chain β el
linkMap f (Middle el) = Middle el
linkMap f (Couple a μ z) = Couple a (f a μ z) z
linkMap f (Ends a α m ω z)
    = Ends a (f a α (leftEnd m)) (linkMap f m) (f (rightEnd m) ω z) z

linksZipWith :: (ζ->α->β) -> [ζ] -> Chain α el -> Chain β el
linksZipWith c lnsZipt chain = fromLists (zipWith c lnsZipt links) elements
 where (links, elements) = toLists chain


instance Bifunctor Chain where
  first = linkMap_
  second = fmap

foldc :: (el -> link -> el -> el) -> Chain link el -> el
foldc _ (Middle x) = x
foldc f (Couple a μ z) = f a μ z
foldc f (Ends a α mid ω z) = f a α $ f (foldc f mid) ω z

instance Bifoldable Chain where
  bifold (Middle el) = el
  bifold (Couple a μ z) = a<>μ<>z
  bifold (Ends a α mid ω z) = a<>α<>bifold mid<>ω<>z
  
  bifoldr _ g c (Middle el) = g el c
  bifoldr f g c (Couple a μ z) = g a . f μ $ g z c
  bifoldr f g c (Ends a α mid ω z) = g a . f α $ bifoldr f g (f ω $ g z c) mid
  
  bifoldl _ g c (Middle el) = g c el
  bifoldl f g c (Couple a μ z) = g( f( g c a ) μ ) z
  bifoldl f g c (Ends a α mid ω z) = g( f( bifoldl f g (f( g c a ) α ) mid ) ω ) z

connect :: Chain link el -> link -> Chain link el -> Chain link el
connect (Middle a) α (Middle z) = Couple a α z
connect (Middle a) α (Couple m β z) = Ends a α (Middle m) β z
connect (Couple a α b) β (Middle z) = Ends a α (Middle b) β z
connect (Middle a) α (Ends b β right ω z) = Ends a α (chainConsl b β right) ω z
connect (Ends a α left ψ y) ω (Middle z) = Ends a α (chainConsr left ψ y) ω z
connect (Couple a α b) β (Ends c γ right ω z) = Ends a α (chainConsl b β $ chainConsl c γ right) ω z
connect (Ends a α left χ x) ψ (Couple y ω z) = Ends a α (chainConsr (chainConsr left χ x) ψ y) ω z
connect (Ends a α left μ l) ν (Ends m ξ right ω z)
       = Ends a α (connect3 left μ (Couple l ν m) ξ right) ω z
       
connect3 :: Chain link el -> link -> Chain link el -> link -> Chain link el
                                            -> Chain link el
connect3 (Middle a) α mid ω (Middle z) = Ends a α mid ω z
connect3 (Middle a) α mid ψ (Couple y ω z) = Ends a α (chainConsr mid ψ y) ω z
connect3 (Couple a α b) β mid ω (Middle z) = Ends a α (chainConsl b β mid) ω z
connect3 (Couple a α b) β mid ψ (Couple y ω z) = Ends a α (Ends b β mid ψ y) ω z
connect3 (Middle a) α mid σ (Ends p τ right ω z)
     = Ends a α (connect3 mid σ (Middle p) τ right) ω z
connect3 (Ends a α left η g) θ mid ω (Middle z)
     = Ends a α (connect3 left η (Middle g) θ mid) ω z
connect3 (Couple a α b) β mid σ (Ends p τ right ω z)
     = Ends a α (connect (Ends b β mid σ p) τ right) ω z
connect3 (Ends a α left η h) θ mid ψ (Couple y ω z)
     = Ends a α (connect left η $ Ends h θ mid ψ y) ω z
connect3 (Ends a α left κ k) λ mid ρ (Ends p σ right ω z)
     = Ends a α (connect3 left κ (Ends k λ mid ρ p) σ right) ω z
