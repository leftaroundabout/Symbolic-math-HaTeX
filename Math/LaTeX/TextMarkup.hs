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
instance MatchesToLaTeX RegexMatchToLaTeX where
  matchesToLaTeX = id
instance MatchesToLaTeX LaTeX where
  matchesToLaTeX = MatchesToLaTeX . const
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


type Regex = String
type MarkupRule = (String, RegexMatchToLaTeX)

data TextMarkupConfig = TextMarkupConfig
  { regexsToLaTeX :: [MarkupRule] }


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
  [ (     "__([^_]*)__"    , matchesToLaTeX (textbf::LaTeXFn)   )  -- __Bold face__, rather unusual form
  , (    "_([^_][^_]*)_"   , matchesToLaTeX (textit::LaTeXFn)   )  -- For text _emphasised_ through italic.
  , ( "\\*\\*([^*]*)\\*\\*", matchesToLaTeX (textbf::LaTeXFn)   )  -- (you'd generally use the **asterisk variant**)
  , (  "\\*([^*][^*]*)\\*" , matchesToLaTeX (emph::LaTeXFn)     )  -- Emphasised *normally* also through italic, but depends on the LaTeX config.
  , (    "`([^`][^`]*)`"   , matchesToLaTeX (verb::Text->LaTeX) )  -- Verbatim code; calls the `verb` macro.
  ]

noTextMarkup :: TextMarkupConfig
noTextMarkup = TextMarkupConfig []


-- | Note that the functions in this class may act either covariantly (when directly
-- dealing with a configuration data type) or contravariantly (for reader monads).
class HasTextMarkupConfig h where
  modifyMarkupRules :: ([MarkupRule]->[MarkupRule]) -> h -> h
  addMarkupRule :: MatchesToLaTeX m => String -> m -> h -> h
  addMarkupRule rgex mtfn = modifyMarkupRules 
        ( ++ [(rgex, matchesToLaTeX mtfn)] )
  resetMarkupRules :: HasTextMarkupConfig h => h -> h
  resetMarkupRules = modifyMarkupRules $ const []


instance HasTextMarkupConfig TextMarkupConfig where
  modifyMarkupRules f = TextMarkupConfig . f . regexsToLaTeX


