-- |
-- Module      : Math.LaTeX.TextMarkup
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 

{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE FlexibleContexts                 #-}

module Math.LaTeX.TextMarkup where

import Text.LaTeX
import Text.LaTeX.Base

import Text.Regex.TDFA
import Data.Text(Text)

import Control.Monad.Reader
import Data.Foldable


data RegexMatchToLaTeX
  = MatchesToLaTeX           ([String] -> LaTeX)
  | MatchesToLaTeX_Recursive ([LaTeX] -> LaTeX)

class MatchesToLaTeX m where
  matchesToLaTeX :: m -> RegexMatchToLaTeX
instance MatchesToLaTeX ([String] -> LaTeX) where
  matchesToLaTeX = MatchesToLaTeX
instance MatchesToLaTeX ([Text] -> LaTeX) where
  matchesToLaTeX = MatchesToLaTeX . (.map fromString)
instance MatchesToLaTeX ([LaTeX] -> LaTeX) where
  matchesToLaTeX = MatchesToLaTeX_Recursive
instance MatchesToLaTeX (String -> LaTeX) where
  matchesToLaTeX = MatchesToLaTeX . foldMap
instance MatchesToLaTeX (Text -> LaTeX) where
  matchesToLaTeX = MatchesToLaTeX . foldMap . (.fromString)
instance MatchesToLaTeX (LaTeX -> LaTeX) where
  matchesToLaTeX = MatchesToLaTeX_Recursive . foldMap
 

data TextMarkupConfig = TextMarkupConfig
  { regexsToLaTeX :: [(String, RegexMatchToLaTeX)] }


markupTxtToLaTeX :: MonadReader TextMarkupConfig m => String -> m LaTeX
markupTxtToLaTeX rawString = do
   mupCfg <- ask
   let allRgxs = regexsToLaTeX mupCfg
       go str [] = fromString str
       go str rgxs@((regex, renderer) : rgxs')
          = case str =~ regex of
             (stragain, "", "", []) -> go stragain rgxs'
             (before, _, after, matchGrps)
                 -> go before rgxs' <> run renderer matchGrps <> go after rgxs
       run (MatchesToLaTeX f) = f
       run (MatchesToLaTeX_Recursive f) = f . map (`go`allRgxs)
   return $ go rawString allRgxs

type LaTeXFn = LaTeX->LaTeX

inlineMarkdown :: TextMarkupConfig
inlineMarkdown = TextMarkupConfig
  [ (    "_([^_][^_]*)_"   , matchesToLaTeX (textit::LaTeXFn)     )
  , (  "\\*([^*][^*]*)\\*" , matchesToLaTeX (emph::LaTeXFn)       )
  , (     "__([^_]*)__"    , matchesToLaTeX (textbf::LaTeXFn)     )
  , ( "\\*\\*([^*]*)\\*\\*", matchesToLaTeX (textbf::LaTeXFn)     )
  , (    "`([^`][^`]*)`"   , matchesToLaTeX (verb::Text->LaTeX) )
  ]

noTextMarkup :: TextMarkupConfig
noTextMarkup = TextMarkupConfig []
