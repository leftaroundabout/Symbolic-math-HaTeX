-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LaTeXComparer where


import Math.LaTeX.Internal.MathExpr
import CAS.Dumb

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Text.LaTeX (LaTeX, Text)


type SLaTeX = Text

data TestTree σ = TestGroup String [TestTree σ]
                | TestCase (LaTeXMath σ) String SLaTeX


mkLaTeXSnip :: QuasiQuoter
mkLaTeXSnip = QuasiQuoter procExpr undefined undefined undefined
 where procExpr e = return $ case parseExp e of
           Right exp -> ConE 'TestCase `AppE` exp `AppE` LitE (StringL e)
           Left perr -> error perr
