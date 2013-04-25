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
         , MathematicalLaTeX
         , MathematicalLaTeX_
         , MathematicalLaTeXT
         , MathematicalLaTeXT_
         , TeXMathStateProps(..)
         , texMathGroundState
         , srcNLEnv
         ) where

import Math.LaTeX.RendConfig

import Text.LaTeX.Base
import Text.LaTeX.Base.Class

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Data.List
import Data.Function
import Data.String



data TeXMathStateProps = TeXMathStateProps {
   punctuationNeededAtDisplayEnd :: Maybe String
 }

texMathGroundState :: TeXMathStateProps
texMathGroundState = TeXMathStateProps {
   punctuationNeededAtDisplayEnd = Nothing
 }


type MathematicalLaTeXT m a = StateT TeXMathStateProps (
                              ReaderT TeXMathConfiguration (LaTeXT m) ) a
type MathematicalLaTeXT_ m = MathematicalLaTeXT m ()  -- ReaderT TeXMathDisplayConf (LaTeXT m) ()
type MathematicalLaTeX a = MathematicalLaTeXT Identity a  -- ReaderT TeXMathDisplayConf (LaTeXT Identity) a
type MathematicalLaTeX_ = MathematicalLaTeXT Identity () -- ReaderT TeXMathDisplayConf (LaTeXT Identity) ()

instance (Monad m) => IsString (MathematicalLaTeXT m a) where
  fromString s = do
     (TeXMathStateProps {..}) <- get
     lift . lift . fromString $ case punctuationNeededAtDisplayEnd of
        Just pnct -> pnct ++ " " ++ s
        Nothing   -> s

srcNLEnv :: LaTeX -> LaTeX
srcNLEnv e = raw"\n" <> e <> raw"\n"
                               


wDefaultConf_toHaTeX :: Monad m => MathematicalLaTeXT m a -> LaTeXT m a
wDefaultConf_toHaTeX = (`runReaderT` mathLaTeXExprDefaultConfig)
               . liftM fst . (`runStateT`texMathGroundState)
