-- |
-- Module      : Math.LaTeX.Internal.Display
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>7 extensions
-- 

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE CPP                 #-}

module Math.LaTeX.Internal.Display where

import qualified Text.LaTeX as LaTeX
import Text.LaTeX (raw)
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX)
import qualified Text.LaTeX.Base.Class as LaTeX
import qualified Text.LaTeX.Base.Types as LaTeX
import qualified Text.LaTeX.Base.Commands as LaTeX
import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv, TeXComm))
import qualified Text.LaTeX.Packages.AMSMath as LaTeX
import qualified Text.LaTeX.Packages.AMSFonts as LaTeX

import CAS.Dumb
import CAS.Dumb.Tree
import CAS.Dumb.Symbols
import CAS.Dumb.LaTeX.Symbols
import Math.LaTeX.Internal.MathExpr

import Data.Foldable (fold)
import Data.Monoid ((<>))
import Control.Arrow
import Data.String (fromString)
import Data.Char (isAlpha)


infixl 1 >$
-- | Embed inline maths in a monadic chain of document-components. Space before
--   the math is included automatically.
--
-- @
--   do
--     \"If\">$𝑎;" and">$𝑏;" are the lengths of the legs and">$𝑐
--     " of the cathete of a right triangle, then">$ 𝑎◝2+𝑏◝2 ⩵ 𝑐◝2;" holds."
-- @
(>$) :: (LaTeXC r, SymbolClass σ, SCConstraint σ LaTeX)
        => r -> CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX) -> r
s >$ m = s <> " " <> LaTeX.math (toMathLaTeX' m)

infixr 6 $<>
-- | Embed inline maths in a semigroup/monoidal chain of document-components.
--
-- @
--     "If "<>𝑎$<>" and "<>𝑏$<>" are the lengths of the legs and "<>𝑐$<>
--      " of the cathete of a right triangle, then "<>(𝑎◝2+𝑏◝2 ⩵ 𝑐◝2)$<>" holds."
-- @
($<>) :: (LaTeXC r, SymbolClass σ, SCConstraint σ LaTeX)
        => CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX) -> r -> r
m $<> s = LaTeX.math (toMathLaTeX' m) <> s

-- | Include a formula / equation system as a LaTeX display. If it's a single
--   equation, automatic line breaks are inserted (requires the
--   <https://www.ctan.org/pkg/breqn?lang=en breqn LaTeX package>).
dmaths :: (LaTeXC r, SymbolClass σ, SCConstraint σ LaTeX)
   => [[CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)]]
               -- ^ Equations to show.
    -> String  -- ^ “Terminator” – this can include punctuation (when an equation
               --   is at the end of a sentence in the preceding text).
    -> r
dmaths [[e]] garnish = case eqnum of
    Nothing -> fromLaTeX . TeXEnv "dmath*" [] $ toMathLaTeX e <> terminator
    Just n  -> fromLaTeX . TeXEnv "equation" [] $ n <> toMathLaTeX e <> terminator
 where (eqnum, terminator) = parseEqnum garnish
dmaths eqLines garnish = fromLaTeX . TeXEnv
           (case eqnum of{Nothing->"align*";Just _->"align"}) [] $ stack eqLines
 where stack [singline] = fold eqnum <> aliLine singline <> terminator
       stack (line : lines) = aliLine line <> LaTeX.lnbk <> stack lines
       aliLine [] = mempty
       aliLine [q] = contentsWithAlignAnchor q
       aliLine (q : cols)
             = contentsWithAlignAnchor q LaTeX.& aliLine cols
       (eqnum, terminator) = parseEqnum garnish

-- | Include a set of equations or formulas, each with a LaTeX label that can be
--   referenced with 'LaTeX.ref'. (The label name will /not/ appear in the rendered
--   document output; by default it will be just a number but you can tweak it with
--   the terminator by including the desired tag in parentheses.)
equations :: (LaTeXC r, SymbolClass σ, SCConstraint σ LaTeX)
  => [(CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX), String)]
              -- ^ Equations to show, with label name.
   -> String  -- ^ “Terminator” – this can include punctuation (when an equation
              --   is at the end of a sentence in the preceding text).
   -> r
equations [(e,lbl)] garnish = fromLaTeX . TeXEnv "equation" []
          $ maybe mempty id eqnum <> toMathLaTeX e <> terminator <> asSafeLabel lbl
 where (eqnum, terminator) = parseEqnum garnish
equations eqLines garnish = fromLaTeX . TeXEnv "align" [] $ stack eqLines
 where stack [singline] = fold eqnum <> aliLine singline <> terminator
       stack (line : lines) = aliLine line <> LaTeX.lnbk <> stack lines
       aliLine (q, lbl) = contentsWithAlignAnchor q <> asSafeLabel lbl
       terminator :: LaTeX
       (eqnum, terminator) = parseEqnum garnish

asSafeLabel :: String -> LaTeX
asSafeLabel = LaTeX.label . fromString . filter isAlpha

-- | Include a formula / equation system as a LaTeX display.
maths :: (LaTeXC r, SymbolClass σ, SCConstraint σ LaTeX)
  => [[CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)]]
              -- ^ Equations to show.
   -> String  -- ^ “Terminator” – this can include punctuation (when an equation
              --   is at the end of a sentence in the preceding text).
   -> r
maths [[e]] garnish = case eqnum of
   Nothing -> fromLaTeX . TeXEnv "equation*" [] $ toMathLaTeX e <> terminator
   Just n  -> fromLaTeX . TeXEnv "equation" [] $ n <> toMathLaTeX e <> terminator
 where (eqnum, terminator) = parseEqnum garnish
maths eqLines garnish = fromLaTeX . TeXEnv
           (case eqnum of{Nothing->"align*";Just _->"align"}) [] $ stack eqLines
 where stack [singline] = fold eqnum <> aliLine singline <> terminator
       stack (line : lines) = aliLine line <> LaTeX.lnbk <> stack lines
       aliLine [] = mempty
       aliLine [q] = contentsWithAlignAnchor q
       aliLine (q : cols)
             = contentsWithAlignAnchor q LaTeX.& aliLine cols
       (eqnum, terminator) = parseEqnum garnish

-- | Display an equation and also extract the final result. As with 'dmaths', automatic
--   line breaks are inserted by <https://www.ctan.org/pkg/breqn?lang=en breqn>.
dcalculation :: (LaTeXC (m ()), SymbolClass σ, SCConstraint σ LaTeX, Functor m)
  => CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX)
              -- ^ Computation chain to display.
   -> String  -- ^ “Terminator” – this can include punctuation (when an equation
              --   is at the end of a sentence in the preceding text).
   -> m (CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX))
              -- ^ Yield the rightmost expression in the displayed computation
              --   (i.e. usually the final result in a chain of algebraic equalities).
dcalculation ch garnish = fmap (\() -> result) $ case eqnum of
    Nothing -> fromLaTeX . TeXEnv "dmath*" [] $ toMathLaTeX ch <> terminator
    Just n  -> fromLaTeX . TeXEnv "equation" [] $ n <> toMathLaTeX ch <> terminator
 where (eqnum, terminator) = parseEqnum garnish
       result = case ch of
        OperatorChain _ ((_,r):_) -> r
        r -> r

parseEqnum :: LaTeXC r => String -> (Maybe r, r)
parseEqnum [] = (Nothing, mempty)
parseEqnum ('.':n) = second ("."<>) $ parseEqnum n
parseEqnum (',':n) = second (","<>) $ parseEqnum n
parseEqnum ('!':n) = second ("!"<>) $ parseEqnum n
parseEqnum ('?':n) = second ("?"<>) $ parseEqnum n
parseEqnum (';':n) = second (";"<>) $ parseEqnum n
parseEqnum (':':n) = second (raw"{:}"<>) $ parseEqnum n
parseEqnum ('(':n) = ( Just $ raw"\\tag{"<>fromString num<>raw"}"
                     , snd $ parseEqnum r )
 where (num,')':r) = break (==')') n
parseEqnum (c:n) = parseEqnum n


contentsWithAlignAnchor :: (LaTeXC c, SymbolClass σ, SCConstraint σ LaTeX)
      => CAS (Infix LaTeX) (Encapsulation LaTeX) (SymbolD σ LaTeX) -> c
contentsWithAlignAnchor (OperatorChain lc rcs@(_:_))
    = toMathLaTeX' lc <> fromLaTeX op
         <> raw"\\:"LaTeX.&toMathLaTeX' (OperatorChain rc₀ $ init rcs)
 where (Infix _ op, rc₀) = last rcs
contentsWithAlignAnchor (Operator (Infix _ op) lc rc)
    = toMathLaTeX' lc <> fromLaTeX op <> raw"\\:"LaTeX.&toMathLaTeX' rc
contentsWithAlignAnchor q = raw"\\:" LaTeX.& toMathLaTeX' q
