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

module Main where

import LaTeXComparer

import Math.LaTeX.Internal.MathExpr
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
     , [mkLaTeXSnip|      (𝑎 + 𝑏) * 𝑐 |] "\\left(a+b\\right){\\cdot}c"
     , [mkLaTeXSnip|(𝑎 + 𝑏) / (𝑥 - 𝑦) |] "\\frac{a+b}{x-y}"
     , [mkLaTeXSnip| (𝑎 + 𝑏)**(𝑥 - 𝑦) |] "\\left(a+b\\right)^{x-y}"
     , [mkLaTeXSnip|         (𝑝/𝑞)**γ |] "\\left(\\frac{p}{q}\\right)^{\\gamma{}}"
     , [mkLaTeXSnip|      abs(𝑝/𝑞)**ξ |] "\\left|\\frac{p}{q}\\right|^{\\xi{}}"
     , [mkLaTeXSnip|          𝑎**𝑏**𝑐 |] "a^{b^{c}}"
     , [mkLaTeXSnip|        (𝑎**𝑏)**𝑐 |] "\\left(a^{b}\\right)^{c}"
     , [mkLaTeXSnip|      sin (sin 𝑥) |] "\\sin{\\left(\\sin{x}\\right)}"
     , [mkLaTeXSnip|       (𝑖⩵0,3)∑ 𝑖 |] "\\sum_{i=0}^{3} i"
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
     , testGroup "Logical"
        [ [mkLaTeXSnip| 𝑝 ∨ 𝑞 |] "p\\vee{}q"
        , [mkLaTeXSnip| 𝑝 ∧ 𝑞 |] "p\\wedge{}q"
        , [mkLaTeXSnip| 𝑝==>𝑞 |] "p\\Longrightarrow q"
        , [mkLaTeXSnip| 𝑝<==𝑞 |] "p\\Longleftarrow q"
        , [mkLaTeXSnip| 𝑝<=>𝑞 |] "p\\Longleftrightarrow q"
        , [mkLaTeXSnip| 𝑝==>𝑞==>𝑟 |] "p\\Longrightarrow q\\Longrightarrow r"
        ]
     , testGroup "Relations"
        [ [mkLaTeXSnip| 𝑎 ⩵ 𝑏 |] "a=b"
        , [mkLaTeXSnip| 𝑎 ≥ 𝑐 |] "a\\geq{}c"
        , [mkLaTeXSnip| 𝑎 ⪡ ρ |] "a<\\rho{}"
        , [mkLaTeXSnip| 𝑥 ⩵ 𝑦 ⩵ 𝑧 |] "x=y=z"
        , [mkLaTeXSnip| 𝑠 ⊂ 𝑡 ⊆ 𝑢 |] "s\\subset{}t\\subseteq{}u"
        ]
     ]
  , testGroup "Calculus"
     [ testGroup "Integration"
        [ [mkLaTeXSnip| (-1,1)∫d 𝑥 (𝑥**2) |] "\\int\\limits_{-1}^{1}\\mathrm{d}x\\ x^{2}"
        , [mkLaTeXSnip| ω◞∫d 𝑥 (exp $ -(𝑥**2)) |]
              "\\int_{\\omega{}}\\!\\!\\!\\mathrm{d}x\\ \\exp{\\left(-x^{2}\\right)}"
        ]
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
                                else ("| Haskell | LaTeX | pdf |\
                                    \\n| ---: | --- | :--- |\n"<>)) $
           "| `"<>Txt.pack ec
           <>"` | `"<>mkGithubtablesaveCode s
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
       enc '+' = "PLUS"
       enc '-' = "MINUS"
       enc '\\' = "BACKSLASH"
       enc '{' = "OBRACE"
       enc '}' = "CBRACE"
       enc '(' = "OPAREN"
       enc ')' = "CPAREN"
       enc '^' = "TOTHE"
       enc '_' = "SUBSCRIPT"
       enc '|' = "PIPE"
       enc '!' = "BANG"
       enc '=' = "EQUALS"
       enc '<' = "LESSER"
       enc '>' = "GREATER"
       enc ' ' = "SPACE"
       enc c = error $ "Unencodable character '"++[c]++"'"

mkGithubtablesaveCode :: Text -> Text
mkGithubtablesaveCode = Txt.concatMap esc
 where esc '|' = "\\|"
       esc c = Txt.singleton c
