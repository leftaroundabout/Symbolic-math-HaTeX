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

import qualified Data.HashSet as HS
import qualified Data.Hashable as HS

operatorExpression :: Fixity -> s² -> CAS' γ (Infix s²) s¹ s⁰
                -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰
operatorExpression fxty iop = symbolInfix (Infix fxty iop)

chainOperatorExpression :: (Eq s², HS.Hashable s²)
          => Fixity -> HS.HashSet s² -> s² -> CAS' γ (Infix s²) s¹ s⁰
                -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰
chainOperatorExpression fxty caste iop = chainableInfix isInCaste (Infix fxty iop)
 where isInCaste (Infix _ iop') = iop' `HS.member` caste

makeOperatorCaste :: String   -- ^ What to call the collection of operators
                  -> (Name, Name)
                              -- ^ Haskell type of the final operators
                              --   and of the infix primitives therein
                  -> Fixity
                  -> Bool     -- ^ Should the ops in this caste be chainable?
                  -> [(String, ExpQ)]
                              -- ^ The operator symbols with corresponding implementation
                  -> DecsQ
makeOperatorCaste caste (typeSig, opType) fxty isChainable ops
                = fmap concat $ (:) <$> mkCollection
                                    <*> mapM mkOp ops
 where mkCollection
        | isChainable  = do
           opRenditions <- mapM snd ops
           return
            [ SigD casteName $ ConT ''HS.HashSet `AppT` ConT opType
            , ValD (VarP casteName)
                   (NormalB
                     $ VarE 'HS.fromList
                        `AppE` ListE opRenditions )
                   []
            ]
        | otherwise    = return []
       mkOp (op, implementation) = do
        impExp <- implementation
        return
         [ InfixD fxty opName
         , SigD opName $ ConT typeSig
         , ValD (VarP opName)
                (NormalB
                   $ if isChainable
                      then VarE 'chainOperatorExpression
                             `AppE` fxtyE `AppE` VarE casteName `AppE` impExp
                      else VarE 'operatorExpression `AppE` fxtyE `AppE` impExp)
                [] ]
        where opName = mkName op
              fxtyE = case parseExp $ show fxty of
                 Right fe -> fe
                 Left err -> error err
       casteName = mkName caste

