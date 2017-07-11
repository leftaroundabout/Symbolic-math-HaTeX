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

module Main where

import Math.LaTeX.Internal.MathExpr
import Text.LaTeX (LaTeX, raw, Text)
import qualified Text.LaTeX as LaTeX
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt

import CAS.Dumb

import System.FilePath
import System.Directory
import System.Process
import Network.URI.Encode

import Data.Monoid
import Control.Monad


main :: IO ()
main = do
   evalTests tests >>= Txt.writeFile "../../README.md"
   

type Expr = Expression LaTeX


tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Simple expressions"
     [         𝑎 + 𝑏 * 𝑐 ====>$$ "a+b{\\cdot}c"
     ,       (𝑎 + 𝑏) * 𝑐 ====>$$ "\\left(a+b\\right){\\cdot}c"
     , (𝑎 + 𝑏) / (𝑥 - 𝑦) ====>$$ "\\frac{a+b}{x-y}"
     ,  (𝑎 + 𝑏)**(𝑥 - 𝑦) ====>$$ "\\left(a+b\\right)^{x-y}"
     ,           𝑎**𝑏**𝑐 ====>$$ "a^{b^{c}}"
     ,         (𝑎**𝑏)**𝑐 ====>$$ "\\left(a^{b}\\right)^{c}"
     ,       sin (sin 𝑥) ====>$$ "\\sin{\\left(\\sin{x}\\right)}"
     ]
  ]


data TestTree = TestGroup String [TestTree]
              | TestCase Expr Text

testGroup :: String -> [TestTree] -> TestTree
testGroup = TestGroup

infix 0 ====>$$
(====>$$) :: Expr -> Text -> TestTree
(====>$$) = TestCase

evalTests :: TestTree -> IO Text
evalTests = go 1
 where go _ (TestCase e s)
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
                callProcess "convert" ["-density","300", "expression.pdf", snipName<.>"png"]
         return $ Txt.unlines
            [ "#### Haskell"
            , "```haskell"
            , Txt.pack $ show e
            , "```"
            , ""
            , "#### LaTeX"
            , "```latex"
            , s
            , "```"
            ]
        where s' = LaTeX.render (toMathLaTeX e)
       go i (TestGroup g s) = (Txt.pack (replicate i '#' <> " " <> g <> "\n") <>)
                              . Txt.concat <$> mapM (go $ i+1) s


