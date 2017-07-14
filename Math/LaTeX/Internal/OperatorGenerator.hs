-- |
-- Module      : Math.LaTeX.Internal.OperatorGenerator
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE CPP                 #-}

module Math.LaTeX.Internal.OperatorGenerator where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols

import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Data.Char

operatorExpression :: Fixity -> s² -> CAS' γ (Infix s²) s¹ s⁰
                -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰
operatorExpression fxty iop = symbolInfix (Infix fxty iop)

makeOperatorCaste :: String   -- ^ What to call the collection of operators
                  -> Name     -- ^ Type of the operators
                  -> Fixity
                  -> Bool     -- ^ Should the ops in this caste be chainable?
                  -> [(String, ExpQ)]
                              -- ^ The operator symbols with corresponding implementation
                  -> DecsQ
makeOperatorCaste caste typeSig fxty isChainable ops
                = fmap concat $ (:) <$> mkCollection
                                    <*> mapM mkOp ops
 where mkCollection
        | isChainable  = undefined
        | otherwise    = return []
       mkOp (op, implementation) = do
        impExp <- implementation
        return
         [ InfixD fxty opName
         , SigD opName $ ConT typeSig
         , ValD (VarP opName)
                (NormalB
                   $ VarE 'operatorExpression `AppE` fxtyE `AppE` impExp)
                [] ]
        where opName = mkName op
              fxtyE = case parseExp $ show fxty of
                 Right fe -> fe
                 Left err -> error err

