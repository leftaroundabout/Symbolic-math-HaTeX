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
                           "𝑎+𝑏*𝑐"
#if __GLASGOW_HASKELL__ > 801
     , [mkLaTeXSnip|        𝐴 * 𝐵 + 𝐶 |] "A{\\cdot}B+C"
                           "𝐴*𝐵+𝐶"
#endif
     , [mkLaTeXSnip|      (𝑎 + 𝑏) * 𝑐 |] "\\left(a+b\\right){\\cdot}c"
                         "(𝑎+𝑏)*𝑐"
     , [mkLaTeXSnip|(𝑎 + 𝑏) / (𝑥 - 𝑦) |] "\\frac{a+b}{x-y}"
                   "(𝑎+𝑏)/(𝑥-𝑦)"
     , [mkLaTeXSnip| (𝑎 + 𝑏)**(𝑥 - 𝑦) |] "\\left(a+b\\right)^{x-y}"
                    "(𝑎+𝑏)◝(𝑥-𝑦)"
     , [mkLaTeXSnip|         (𝑝/𝑞)**γ |] "\\left(\\frac{p}{q}\\right)^{\\gamma{}}"
                            "(𝑝/𝑞)◝γ"
     , [mkLaTeXSnip|      abs(𝑝/𝑞)**ξ |] "\\left|\\frac{p}{q}\\right|^{\\xi{}}"
                         "abs (𝑝/𝑞)◝ξ"
     , [mkLaTeXSnip|          𝑎**𝑏**𝑐 |] "a^{b^{c}}"
                             "𝑎◝𝑏◝𝑐"
     , [mkLaTeXSnip|        (𝑎**𝑏)**𝑐 |] "\\left(a^{b}\\right)^{c}"
                           "(𝑎◝𝑏)◝𝑐"
     , [mkLaTeXSnip|      sin (sin 𝑥) |] "\\sin{\\left(\\sin{x}\\right)}"
                         "sin (sin 𝑥)"
     , [mkLaTeXSnip|       (𝑖⩵0,3)∑ 𝑖 |] "\\sum_{i=0}^{3} i"
                          "(𝑖⩵0,3)∑𝑖"
     , [mkLaTeXSnip|   matrix[[ 0,1]
                             ,[-1,0]] |] "\\begin{pmatrix}0&1\\\\ -1&0\\end{pmatrix}"
                      "matrix[[0,1],[-1,0]]"
     ]
  , testGroup "Number literals"
     [ [mkLaTeXSnip| 25697325 |] "25697325"
                    "25697325"
     , [mkLaTeXSnip|    4.718 |] "4.718"
                       "4.718"
     , [mkLaTeXSnip|     1e-3 |] "1{\\cdot}10^{ -3}"
                        "1e-3"
     , [mkLaTeXSnip| 257.35e9 |] "2.5735{\\cdot}10^{11}"
                    "2.5735e11"
     , [mkLaTeXSnip|  -5.1e-8 |] " -5.1{\\cdot}10^{ -8}"
                     "-5.1e-8"
     , [mkLaTeXSnip|     7/13 |] "\\frac{7}{13}"
                        "7/13"
     , [mkLaTeXSnip|   -(1/2) |] " -\\frac{1}{2}"
                      "-(1/2)"
     ]
  , testGroup "Operators"
     [ testGroup "Arithmetic"
        [ [mkLaTeXSnip| 𝑎 + 𝑏 |] "a+b"
                       "𝑎+𝑏"
        , [mkLaTeXSnip| 𝑎 - 𝑏 |] "a-b"
                       "𝑎-𝑏"
        , [mkLaTeXSnip| 𝑎 * 𝑏 |] "a{\\cdot}b"
                       "𝑎*𝑏"
        , [mkLaTeXSnip| 𝑎 × 𝑏 |] "a\\times{}b"
                       "𝑎×𝑏"
        , [mkLaTeXSnip| 𝑎 ± 𝑏 |] "a\\pm{}b"
                       "𝑎±𝑏"
        , [mkLaTeXSnip| 𝑎 ∓ 𝑏 |] "a\\mp{}b"
                       "𝑎∓𝑏"
        , [mkLaTeXSnip| 𝑎 ⊕ 𝑏 |] "a\\oplus{}b"
                       "𝑎⊕𝑏"
        , [mkLaTeXSnip| 𝑎 ⊗ 𝑏 |] "a\\otimes{}b"
                       "𝑎⊗𝑏"
        ]
     , testGroup "Sub/superscripts"
        [ [mkLaTeXSnip|         𝑎◞𝑏 |] "a_{b}"
                               "𝑎◞𝑏"
        , [mkLaTeXSnip|    𝑎◞◝(𝑏,𝑐) |] "a_{b}^{c}"
                          "𝑎◞◝(𝑏,𝑐)"
        , [mkLaTeXSnip|     ψ◞"Foo" |] "\\psi{}_{\\mathrm{Foo}}"
                           "ψ◞\"Foo\""
#if __GLASGOW_HASKELL__ > 801
        , [mkLaTeXSnip|     ψ◞𝐹⁀𝑜⁀𝑜 |] "\\psi{}_{Foo}"
                           "ψ◞𝐹⁀𝑜⁀𝑜"
        , [mkLaTeXSnip|      𝑓◝⁀3°𝑥 |] "f^{\\left(3\\right)}\\left(x\\right)"
                            "𝑓◝⁀3°𝑥"
#endif
        ]
     , testGroup "Function application"
        [ [mkLaTeXSnip|         𝑓°𝑥 |] "f\\left(x\\right)"
                               "𝑓°𝑥"
#if __GLASGOW_HASKELL__ > 801
        , [mkLaTeXSnip|     𝑓°(𝑥،𝑦) |] "f\\left(x,y\\right)"
                           "𝑓°(𝑥،𝑦)"
#endif
        ]
     , testGroup "Logical"
        [ [mkLaTeXSnip| 𝑝 ∨ 𝑞 |] "p\\vee{}q"
                       "𝑝∨𝑞"
        , [mkLaTeXSnip| 𝑝 ∧ 𝑞 |] "p\\wedge{}q"
                       "𝑝∧𝑞"
        , [mkLaTeXSnip| 𝑝==>𝑞 |] "p\\Longrightarrow{}q"
                       "𝑝==>𝑞"
        , [mkLaTeXSnip| 𝑝<==𝑞 |] "p\\Longleftarrow{}q"
                       "𝑝<==𝑞"
        , [mkLaTeXSnip| 𝑝<=>𝑞 |] "p\\Longleftrightarrow{}q"
                       "𝑝<=>𝑞"
        , [mkLaTeXSnip| 𝑝==>𝑞==>𝑟 |] "p\\Longrightarrow{}q\\Longrightarrow{}r"
                       "𝑝==>𝑞==>𝑟"
        , [mkLaTeXSnip| cases[(1, "Today"), (2, "Else")] |]
                           "\\begin{cases}1&\\text{Today}\\\\2&\\text{Else}\\end{cases}"
                       "cases[(1,\"Today\"),(2,\"Else\")]"
        ]
     , testGroup "Relations"
        [ [mkLaTeXSnip| 𝑎 ⩵ 𝑏 |] "a=b"
                       "𝑎⩵𝑏"
        , [mkLaTeXSnip| 𝑎 ≥ 𝑐 |] "a\\geq{}c"
                       "𝑎≥𝑐"
        , [mkLaTeXSnip| 𝑎 ⪡ ρ |] "a<\\rho{}"
                       "𝑎⪡ρ"
        , [mkLaTeXSnip| 𝑥 ⩵ 𝑦 ⩵ 𝑧 |] "x=y=z"
                       "𝑥 ⩵ 𝑦 ⩵ 𝑧"
        , [mkLaTeXSnip| 𝑠 ⊂ 𝑡 ⊆ 𝑢 |] "s\\subset{}t\\subseteq{}u"
                       "𝑠⊂𝑡⊆𝑢"
        , [mkLaTeXSnip| ℎ ≈ 𝑔 ∼ 𝑓 ≃ 𝑒 ≅ 𝑑 |] "h\\approx{}g\\sim{}f\\simeq{}e\\cong{}d"
                       "ℎ≈𝑔∼𝑓≃𝑒≅𝑑"
#if __GLASGOW_HASKELL__ > 801
        , [mkLaTeXSnip| 𝑝 ∈ ℚ ⊂ ℝ |] "p\\in{}\\mathbb{Q}\\subset{}\\mathbb{R}"
                       "𝑝∈ℚ⊂ℝ"
#endif
        , [mkLaTeXSnip| 𝐮 ⟂ (vec%$>𝑣) ∥ (underline%$>𝑤) |]
               "\\mathbf{u}\\perp{}\\vec{v}\\parallel{}\\underline{w}"
                       "𝐮⟂(vec%$>𝑣)∥(underline%$>𝑤)"
        ]
     ]
  , testGroup "Calculus"
     [ testGroup "Integration"
        [ [mkLaTeXSnip| (-1,1)∫d 𝑥 (𝑥**2) |] "\\int\\limits_{ -1}^{1}\\mathrm{d}x\\ {}x^{2}"
                       "(-1,1)∫d 𝑥(𝑥**2)"
        , [mkLaTeXSnip| ω◞∫d 𝑥 (exp $ -(𝑥**2)) |]
              "\\int_{\\omega{}}\\!\\!\\!\\mathrm{d}x\\ {}\\exp{\\left( -x^{2}\\right)}"
                       "ω◞∫d 𝑥(exp(-(𝑥**2)))"
        , [mkLaTeXSnip| (0,1)∫d 𝑥 ((0,1)∫d 𝑦 (𝑥*𝑦)) |]
              "\\int\\limits_{0}^{1}\\mathrm{d}x\\ {}\\int\\limits_{0}^{1}\\mathrm{d}y\\ {}\\left(x{\\cdot}y\\right)"
                       "(0,1)∫d 𝑥((0,1)∫d 𝑦(𝑥*𝑦))"
        ]
     ]
  , testGroup "Algebraic manipulation"
     [ [mkLaTeXSnip| 𝑎 + 𝑏 + 𝑐 &~~! [𝑏 ⩵ 𝑦] |]
         "a+b+c=a+y+c" "𝑎+𝑏+𝑐⩵𝑎+𝑦+𝑐"
     , [mkLaTeXSnip| 𝑎 + 𝑏 + 𝑐 &~~! [𝑏+𝑐 ⩵ 𝑐+𝑏, 𝑎+𝑐 ⩵ ξ] |]
         "a+b+c=\\xi{}+b" "𝑎+𝑏+𝑐⩵ξ+𝑏"
     , [mkLaTeXSnip| 𝑎 - 𝑏 &~~! [𝑏 ⩵ 𝑦] &~~! [𝑎 ⩵ 𝑧] |]
         "a-b=a-y=z-y" "𝑎-𝑏⩵𝑎-𝑦⩵𝑧-𝑦"
     , [mkLaTeXSnip| 𝑥 + 𝑦
                  & continueExpr (⩵) (&~: 𝑦 :=: 𝑥*(1+𝑥))
                  & continueExpr (⩵) (&~: 𝑥 :=: 2◝𝑝) |]
         "x+y=x+x{\\cdot}\\left(1+x\\right)=2^{p}+2^{p}{\\cdot}\\left(1+2^{p}\\right)"
         "𝑥+𝑦⩵𝑥+𝑥*(1+𝑥)⩵2◝𝑝+2◝𝑝*(1+2◝𝑝)"
     ]
  , testGroup "Juxtaposition"
     [ [mkLaTeXSnip| 𝑚 + 𝑝⁀𝑞⁀𝑟 |]
         "m+pqr"    "𝑚+𝑝⁀𝑞⁀𝑟"
     , [mkLaTeXSnip| 𝑚 + 𝑝⁀(2+𝑞)⁀𝑟 |]
         "m+p\\left(2+q\\right)r"
                    "𝑚+𝑝⁀(2+𝑞)⁀𝑟"
     , [mkLaTeXSnip| 𝑚 + (𝑝␣𝑞␣𝑟) |]
         "m+\\left(p\\ {}q\\ {}r\\right)"
                    "𝑚+(𝑝␣𝑞␣𝑟)"
     , [mkLaTeXSnip| 𝑚 + (𝑝␣2+𝑞␣𝑟) |]
         "m+\\left(p\\ {}2+q\\ {}r\\right)"
                    "𝑚+(𝑝␣2+𝑞␣𝑟)"
     , [mkLaTeXSnip| 𝑚 + (𝑝<>𝑞<>𝑟) |]
         "m+pqr"
                    "𝑚+(𝑝<>𝑞<>𝑟)"
     , [mkLaTeXSnip| 𝑚 + (𝑝<>(2+𝑞)<>𝑟) |]
         "m+\\left(p2+qr\\right)"
                    "𝑚+(𝑝<>(2+𝑞)<>𝑟)"
     , [mkLaTeXSnip| 𝑚 * ((1+2)<>(3+4)) |]
         "m{\\cdot}\\left(1+23+4\\right)"
                    "𝑚*((1+2)<>(3+4))"
     ]
  , testGroup "Misc"
     [ [mkLaTeXSnip| 3*𝑧 - 1 |]
         "3{\\cdot}z-1"
                    "3*𝑧-1"
     , [mkLaTeXSnip| 𝑎-𝑏+𝑐 |]
         "a-b+c"
                    "𝑎-𝑏+𝑐"
     , [mkLaTeXSnip| (𝑥/2)|◞◝(𝑥⩵0,1) |]
         "\\left.\\frac{x}{2}\\right|_{x=0}^{1}"
                    "(𝑥/2)|◞◝(𝑥⩵0,1)"
     , TestCase (3 - 1 &~~! [ ㄒ-ㄗ ⩵ -(ㄗ-ㄒ) ])
          "3 - 1 &~~! [ ㄒ-ㄗ ⩵ -(ㄗ-ㄒ) ]" "3-1= -\\left(1-3\\right)"
               "3-1⩵(-(1-3))"
     ]
  ]


testGroup :: String -> [TestTree] -> TestTree
testGroup = TestGroup

evalTests :: TestTree -> IO Text
evalTests = go False 1
 where go hasHeader _ (TestCase e ec s _)
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
           "| "<>mconcat["`"
                  <>mkGithubtablesaveCode(Txt.pack (dropWhile (==' ') ecl))
                          <>"` " | ecl<-lines ec]
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
