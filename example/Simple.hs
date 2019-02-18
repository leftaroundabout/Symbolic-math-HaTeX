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

type Math = LaTeXMath__MathLatin_RomanGreek__BopomofoGaps

example :: LaTeX
example
 = TeX.title ("A simple example document for the "<>texmymath<>" Haskell package.")
   <> TeX.author "Justus Sagemüller"
   <> TeX.raw "\\usepackage{fontspec}"
   <> TeX.usepackage [] TeX.amsmath
   <> TeX.usepackage [] TeX.amssymb
   <> Babel.uselanguage `id` Babel.English
   <> TeX.document `id`do
     TeX.maketitle
      <> "The "<>texmymath<>" package allows you to write maths formulas with a plaintext"
      <>" syntax that is more human-readable than LaTeX, and more structurally coherent,"
      <>" namely being Haskell source code whose AST represents how the math would"
      <>" actually parse semantically. The simplest example would be arithmetic expressions"
      <>" with number literals, like"
      <>maths[[4+5*6 :: Math]]"."
      <>"Note that parenthesisation is obeyed:"
      <>maths[[(4+5)*6 :: Math]]"."
 where texmymath = TeX.tex<>"-my-Math"

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
   


