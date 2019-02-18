-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE CPP               #-}

module Main where

import Math.LaTeX.Prelude
import Math.LaTeX.StringLiterals

import Text.LaTeX (LaTeX, raw, Text)
import qualified Text.LaTeX as TeX
import qualified Text.LaTeX.Packages.AMSMath as TeX
import qualified Text.LaTeX.Packages.AMSSymb as TeX
import qualified Text.LaTeX.Packages.Babel as Babel
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import Data.Char

import System.FilePath
import System.Directory
import System.Process

import Data.Monoid
import Data.Function ((&))
import Control.Monad
import Data.Functor.Identity


example :: LaTeX
example
 = TeX.title ("A simple example document for the "<>TeX.tex<>"-my-Math Haskell package.")
   <> TeX.author "Justus Sagemüller"
   <> TeX.raw "\\usepackage{fontspec}"
   <> TeX.usepackage [] TeX.amsmath
   <> TeX.usepackage [] TeX.amssymb
   <> Babel.uselanguage `id` Babel.English
   <> TeX.document `id`do
     TeX.maketitle
      <> "Bla bla"

main :: IO ()
main = do
   wdAbs <- makeAbsolute workdir
   createDirectoryIfMissing True wdAbs
   withCurrentDirectory wdAbs $ do
      TeX.renderFile (thisDocument<>".tex") $ do
         TeX.raw "\\documentclass{article}"
          <> example
      callProcess "xelatex" [thisDocument]
   putStrLn $ "Output generated in directory "<>wdAbs
 where workdir = "example/outputs"
       thisDocument = "simple"
   


