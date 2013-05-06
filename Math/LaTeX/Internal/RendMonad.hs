-- |
-- Module      : Math.LaTeX.Internal.RendMonad
-- Copyright   : (c) Justus Sagemüller 2013
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions
-- 

{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}



module Math.LaTeX.Internal.RendMonad(
           module Control.Monad.State
         , wDefaultConf_toHaTeX
         , toHaTeX
         , toHaTeX_wConfig
         , MathematicalLaTeX
         , MathematicalLaTeX_
         , MathematicalLaTeXT
         , MathematicalLaTeXT_
         , TeXMathStateProps(..)
         , texMathGroundState
         , srcNLEnv
         ) where

import Math.LaTeX.Config

import Text.LaTeX.Base
import Text.LaTeX.Base.Class

import Math.LaTeX.TextMarkup

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Data.List
import Data.HList
import Data.Function
import Data.String



data TeXMathStateProps = TeXMathStateProps {
   punctuationNeededAtDisplayEnd :: Maybe String
 }

texMathGroundState :: TeXMathStateProps
texMathGroundState = TeXMathStateProps {
   punctuationNeededAtDisplayEnd = Nothing
 }


type MathematicalLaTeXT baseMonad freeVarStack mRe
   = StateT TeXMathStateProps ( ReaderT TeXMathConfiguration
      (LaTeXT baseMonad) ) mRe
type MathematicalLaTeXT_ m = MathematicalLaTeXT m HNil ()
type MathematicalLaTeX f a = MathematicalLaTeXT Identity f a
type MathematicalLaTeX_ f = MathematicalLaTeXT Identity f ()

instance (Monad m) => IsString (MathematicalLaTeXT m f a) where
  fromString s = do
     (TeXMathStateProps {..}) <- get
     mkupCfg <- askTextMarkupConfig
     lift . lift . fromLaTeX . (`runReader`mkupCfg) . markupTxtToLaTeX
       $ case punctuationNeededAtDisplayEnd of
          Just pnct -> pnct ++ " " ++ s
          Nothing   -> s

srcNLEnv :: LaTeX -> LaTeX
srcNLEnv e = raw"\n" <> e <> raw"\n"
                               

toHaTeX :: Monad m => MathematicalLaTeXT m f a -> ReaderT TeXMathConfiguration (LaTeXT m) a
toHaTeX mLaTeX = do
   cfg <- ask
   lift . (`runReaderT` cfg) . liftM fst $ runStateT mLaTeX texMathGroundState

toHaTeX_wConfig :: Monad m => TeXMathConfiguration -> MathematicalLaTeXT m f a -> LaTeXT m a
toHaTeX_wConfig cfg = (`runReaderT`cfg) . toHaTeX

wDefaultConf_toHaTeX :: Monad m => MathematicalLaTeXT m f a -> LaTeXT m a
wDefaultConf_toHaTeX = toHaTeX_wConfig mathLaTeXDefaultConfig


instance (Monad m) => HasTextMarkupConfig (MathematicalLaTeXT m f a) where
  modifyMarkupRules = local . modifyMarkupRules
