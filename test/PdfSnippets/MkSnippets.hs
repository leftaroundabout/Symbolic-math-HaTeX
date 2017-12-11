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

import LaTeXComparer

import Math.LaTeX.Prelude
import Math.LaTeX.StringLiterals

import Text.LaTeX (LaTeX, raw, Text)
import qualified Text.LaTeX as LaTeX
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import Data.Char

import CAS.Dumb

import System.FilePath
import System.Directory
import System.Process

import Data.Monoid
import Data.Function ((&))
import Control.Monad


main :: IO ()
main = do
   examples <- evalTests tests
   Txt.writeFile "EXAMPLES.md"
      $ "_This file was generated automatically from [MkSnippets.hs](test/PdfSnippets/MkSnippets.hs). Run `cabal test` to refresh it._\n"
       <> examples
   


tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Simple expressions"
     [ [mkLaTeXSnip|        𝑎 + 𝑏 * 𝑐 |] "a+b{\\cdot}c"
#if __GLASGOW_HASKELL__ > 801
     , [mkLaTeXSnip|        𝐴 * 𝐵 + 𝐶 |] "A{\\cdot}B+C"
#endif
     , [mkLaTeXSnip|      (𝑎 + 𝑏) * 𝑐 |] "\\left(a+b\\right){\\cdot}c"
     , [mkLaTeXSnip|(𝑎 + 𝑏) / (𝑥 - 𝑦) |] "\\frac{a+b}{x-y}"
     , [mkLaTeXSnip| (𝑎 + 𝑏)**(𝑥 - 𝑦) |] "\\left(a+b\\right)^{x-y}"
     , [mkLaTeXSnip|         (𝑝/𝑞)**γ |] "\\left(\\frac{p}{q}\\right)^{\\gamma{}}"
     , [mkLaTeXSnip|      abs(𝑝/𝑞)**ξ |] "\\left|\\frac{p}{q}\\right|^{\\xi{}}"
     , [mkLaTeXSnip|          𝑎**𝑏**𝑐 |] "a^{b^{c}}"
     , [mkLaTeXSnip|        (𝑎**𝑏)**𝑐 |] "\\left(a^{b}\\right)^{c}"
     , [mkLaTeXSnip|      sin (sin 𝑥) |] "\\sin{\\left(\\sin{x}\\right)}"
     , [mkLaTeXSnip|       (𝑖⩵0,3)∑ 𝑖 |] "\\sum_{i=0}^{3} i"
     , [mkLaTeXSnip|   matrix[[ 0,1]
                             ,[-1,0]] |] "\\begin{pmatrix}0&1\\\\-1&0\\end{pmatrix}"
     ]
  , testGroup "Number literals"
     [ [mkLaTeXSnip| 25697325 |] "25697325"
     , [mkLaTeXSnip|    4.718 |] "4.718"
     , [mkLaTeXSnip|     1e-3 |] "1{\\cdot}10^{-3}"
     , [mkLaTeXSnip| 257.35e9 |] "2.5735{\\cdot}10^{11}"
     , [mkLaTeXSnip|  -5.1e-8 |] "-5.1{\\cdot}10^{-8}"
     , [mkLaTeXSnip|     7/13 |] "\\frac{7}{13}"
     , [mkLaTeXSnip|   -(1/2) |] "-\\frac{1}{2}"
     ]
  , testGroup "Operators"
     [ testGroup "Arithmetic"
        [ [mkLaTeXSnip| 𝑎 + 𝑏 |] "a+b"
        , [mkLaTeXSnip| 𝑎 - 𝑏 |] "a-b"
        , [mkLaTeXSnip| 𝑎 * 𝑏 |] "a{\\cdot}b"
        , [mkLaTeXSnip| 𝑎 × 𝑏 |] "a\\times{}b"
        , [mkLaTeXSnip| 𝑎 ± 𝑏 |] "a\\pm{}b"
        , [mkLaTeXSnip| 𝑎 ∓ 𝑏 |] "a\\mp{}b"
        , [mkLaTeXSnip| 𝑎 ⊕ 𝑏 |] "a\\oplus{}b"
        , [mkLaTeXSnip| 𝑎 ⊗ 𝑏 |] "a\\otimes{}b"
        ]
     , testGroup "Sub/superscripts"
        [ [mkLaTeXSnip|         𝑎◞𝑏 |] "a_{b}"
        , [mkLaTeXSnip|    𝑎◞◝(𝑏,𝑐) |] "a_{b}^{c}"
        , [mkLaTeXSnip|     ψ◞"Foo" |] "\\psi{}_{\\mathrm{Foo}}"
#if __GLASGOW_HASKELL__ > 801
        , [mkLaTeXSnip|     ψ◞𝐹⁀𝑜⁀𝑜 |] "\\psi{}_{Foo}"
#endif
        ]
     , testGroup "Function application"
        [ [mkLaTeXSnip|         𝑓°𝑥 |] "f\\left(x\\right)"
#if __GLASGOW_HASKELL__ > 801
        , [mkLaTeXSnip|     𝑓°(𝑥،𝑦) |] "f\\left(x,y\\right)"
#endif
        ]
     , testGroup "Logical"
        [ [mkLaTeXSnip| 𝑝 ∨ 𝑞 |] "p\\vee{}q"
        , [mkLaTeXSnip| 𝑝 ∧ 𝑞 |] "p\\wedge{}q"
        , [mkLaTeXSnip| 𝑝==>𝑞 |] "p\\Longrightarrow q"
        , [mkLaTeXSnip| 𝑝<==𝑞 |] "p\\Longleftarrow q"
        , [mkLaTeXSnip| 𝑝<=>𝑞 |] "p\\Longleftrightarrow q"
        , [mkLaTeXSnip| 𝑝==>𝑞==>𝑟 |] "p\\Longrightarrow q\\Longrightarrow r"
        , [mkLaTeXSnip| cases[(1, "Today"), (2, "Else")] |]
                           "\\begin{cases}1&\\text{Today}\\\\2&\\text{Else}\\end{cases}"
        ]
     , testGroup "Relations"
        [ [mkLaTeXSnip| 𝑎 ⩵ 𝑏 |] "a=b"
        , [mkLaTeXSnip| 𝑎 ≥ 𝑐 |] "a\\geq{}c"
        , [mkLaTeXSnip| 𝑎 ⪡ ρ |] "a<\\rho{}"
        , [mkLaTeXSnip| 𝑥 ⩵ 𝑦 ⩵ 𝑧 |] "x=y=z"
        , [mkLaTeXSnip| 𝑠 ⊂ 𝑡 ⊆ 𝑢 |] "s\\subset{}t\\subseteq{}u"
#if __GLASGOW_HASKELL__ > 801
        , [mkLaTeXSnip| 𝑝 ∈ ℚ ⊂ ℝ |] "p\\in{}\\mathbb{Q}\\subset{}\\mathbb{R}"
#endif
        ]
     ]
  , testGroup "Calculus"
     [ testGroup "Integration"
        [ [mkLaTeXSnip| (-1,1)∫d 𝑥 (𝑥**2) |] "\\int\\limits_{-1}^{1}\\mathrm{d}x\\ x^{2}"
        , [mkLaTeXSnip| ω◞∫d 𝑥 (exp $ -(𝑥**2)) |]
              "\\int_{\\omega{}}\\!\\!\\!\\mathrm{d}x\\ \\exp{\\left(-x^{2}\\right)}"
        , [mkLaTeXSnip| (0,1)∫d 𝑥 ((0,1)∫d 𝑦 (𝑥*𝑦)) |]
              "\\int\\limits_{0}^{1}\\mathrm{d}x\\ \\int\\limits_{0}^{1}\\mathrm{d}y\\ \\left(x{\\cdot}y\\right)"
        ]
     ]
  , testGroup "Algebraic manipulation"
     [ [mkLaTeXSnip| 𝑎 + 𝑏 + 𝑐 &~~! [𝑏 ⩵ 𝑦] |]
         "a+b+c=a+y+c"
     , [mkLaTeXSnip| 𝑎 + 𝑏 + 𝑐 &~~! [𝑏+𝑐 ⩵ 𝑐+𝑏, 𝑎+𝑐 ⩵ ξ] |]
         "a+b+c=\\xi{}+b"
     , [mkLaTeXSnip| 𝑎 - 𝑏 &~~! [𝑏 ⩵ 𝑦] &~~! [𝑎 ⩵ 𝑧] |]
         "a-b=a-y=z-y"
     , [mkLaTeXSnip| 𝑥 + 𝑦
                  & continueExpr (⩵) (&~: 𝑦 :=: 𝑥*(1+𝑥))
                  & continueExpr (⩵) (&~: 𝑥 :=: 2◝𝑝) |]
         "x+y=x+x{\\cdot}\\left(1+x\\right)=2^{p}+2^{p}{\\cdot}\\left(1+2^{p}\\right)"
     ]
  , testGroup "Juxtaposition"
     [ [mkLaTeXSnip| 𝑚 + 𝑝⁀𝑞⁀𝑟 |]
         "m+pqr"
     , [mkLaTeXSnip| 𝑚 + 𝑝⁀(2+𝑞)⁀𝑟 |]
         "m+p\\left(2+q\\right)r"
     , [mkLaTeXSnip| 𝑚 + (𝑝␣𝑞␣𝑟) |]
         "m+\\left(p\\ q\\ r\\right)"
     , [mkLaTeXSnip| 𝑚 + (𝑝␣2+𝑞␣𝑟) |]
         "m+\\left(p\\ 2+q\\ r\\right)"
     , [mkLaTeXSnip| 𝑚 + (𝑝<>𝑞<>𝑟) |]
         "m+pqr"
     , [mkLaTeXSnip| 𝑚 + (𝑝<>(2+𝑞)<>𝑟) |]
         "m+p2+qr"
     ]
  ]


testGroup :: String -> [TestTree] -> TestTree
testGroup = TestGroup

evalTests :: TestTree -> IO Text
evalTests = go False 1
 where go hasHeader _ (TestCase e ec s)
        | s==s'    = do
         let snipName = "test/PdfSnippets"</>encode (Txt.unpack s)
         doesFileExist (snipName<.>".png") >>= flip
            (when . not)`id` do
                Txt.writeFile ("expression.tex") $ Txt.unlines
                   [ "\\documentclass[border=2pt]{standalone}"
                   , "\\usepackage[utf8x]{inputenc}"
                   , "\\usepackage{amsmath}"
                   , "\\usepackage{amssymb}"
                   , "\\pagestyle{empty}"
                   , "\\begin{document}"
                   , "$"<>s<>"$"
                   , "\\end{document}"
                   ]
                readProcess "pdflatex" ["expression.tex"] ""
                callProcess "convert" [ "-density","300"
                                      , "-background","grey", "-alpha","remove"
                                      , "expression.pdf", snipName<.>"png" ]
         return . (if hasHeader then id
                                else (("| Haskell | LaTeX | pdf |"
                                   <>"\n| ---: | --- | :--- |\n")<>)) $
           "| "<>mconcat["`"<>Txt.pack (dropWhile (==' ') ecl)<>"` " | ecl<-lines ec]
           <>"| `"<>mkGithubtablesaveCode s
           <>"` | ![pdflatex-rendered version of `"<>mkGithubtablesaveCode s
                            <>"`]("<>Txt.pack(snipName<.>"png")<>") |\n"
        | otherwise    = error $ "Got "<>show s'<>"; expected "
                                  <> show s<>", when rendering "<>ec
        where s' = LaTeX.render (toMathLaTeX e)
       go _ i (TestGroup g (s₀:s))
              = (Txt.pack (replicate i '#' <> " " <> g <> "\n") <>)
               . Txt.concat <$> ((:) <$> go False (i+1) s₀
                                     <*> mapM (go True $ i+1) s)



encode :: String -> String
encode = concatMap enc
 where enc c
        | isAlphaNum c = [c]
       enc '+' = "⼦"
       enc '-' = "⼀"
       enc '\\' = "ᓭ"
       enc '{' = "ⶈ"
       enc '}' = "ⶉ"
       enc '(' = "ᑕ"
       enc ')' = "ᑐ"
       enc '^' = "ᐞ"
       enc '_' = "⣀"
       enc '|' = "ᛁ"
       enc '!' = "⢘"
       enc '&' = "ತ"
       enc '=' = "〧"
       enc '<' = "ᐸ"
       enc '>' = "ᐳ"
       enc ',' = "،"
       enc '.' = "៰"
       enc ' ' = "ᐧ"
       enc c = error $ "Unencodable character '"++[c]++"'"

mkGithubtablesaveCode :: Text -> Text
mkGithubtablesaveCode = Txt.concatMap esc
 where esc '|' = "\\|"
       esc c = Txt.singleton c
