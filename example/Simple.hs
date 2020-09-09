-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2019
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
-- Example document. Run `cabal run TeXMyMath-example` in the top-level directory
-- to render it as a PDF.
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
   <> TeX.usepackage [] TeX.amsmath
   <> TeX.usepackage [] TeX.amssymb
   <> TeX.raw "\\usepackage{fontspec}"
   <> TeX.raw "\\usepackage{bigfoot}"
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
      <>"For symbols, we recommend using the Unicode primitives that come by default "
      <>" with the "<>TeX.verb"Math.LaTeX.Prelude"<>" module"
      <>TeX.footnote("These symbols are originally declared in the "<>TeX.verb"dumb-cas"
                   <>" package, in the "
                   <>TeX.verb"CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps"
                   <>" module.")
      <>". This allows symbols like "<>𝑎$<>", "<>𝑏$<>" or "<>ψ$<>" to appear natural and"
      <>" similar to their rendered form in the plaintext source. This includes also bold,"
      <>" “blackboard” etc. variants, and notably allows uppercase characters"
      <>TeX.footnote("Using the GHC "<>TeX.verb"PatternSynonyms"<>" extension as a hack"
                   <>" around Haskell's syntax restriction that identifiers must start with"
                   <>" a lowercase character.")
      <>" in addition to lowercase."
      <>maths[[ 𝑎 + 𝑏 + 𝑀 + 𝑁 + 𝐱 + 𝐲 + ℍ + 𝓠 + 𝓛 + Γ + ω ]]"."
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
      callProcess "convert" [ "-density", "150"
                            , thisDocument<>".pdf"
                            , "-trim"
                            , "-background", "white", "-alpha", "off"
                            , "-resize", "50%"
                            , thisDocument<>".png" ]
   putStrLn $ "Output generated in directory "<>wdAbs
 where workdir = "example/outputs"
       thisDocument = "simple"
   


