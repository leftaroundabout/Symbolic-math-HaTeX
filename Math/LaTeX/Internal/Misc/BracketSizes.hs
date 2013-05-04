-- |
-- Module      : Math.LaTeX.Internal.Misc.BracketSizes
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 

module Math.LaTeX.Internal.Misc.BracketSizes where


import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import qualified Text.LaTeX.Packages.AMSMath as AMS

-- | This doesn't really belong in this module.
type BracketSize = Int

defaultBracketSize = 0 :: BracketSize

brackSizeCommPrefix :: BracketSize -> Maybe String
brackSizeCommPrefix 0 = Nothing
brackSizeCommPrefix n = Just $ case n of
                              1 -> "big"
                              2 -> "Big"
                              3 -> "bigg"
                              4 -> "Bigg"

latexBrackets :: Maybe BracketSize -> String -> String -> LaTeX -> LaTeX
latexBrackets Nothing ld rd inr = AMS.autoBrackets (raw $ fromString ld) (raw $ fromString rd) inr
latexBrackets (Just sz) ld rd inr = sized 'l' ld <> inr <> sized 'r' rd
 where sized = case brackSizeCommPrefix sz of
        Nothing      -> const $ raw . fromString
        Just prefix  -> \sd brk -> commS $ prefix ++ sd : brk
