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
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}



module Math.LaTeX.Internal.RendMonad(
           module Control.Monad.State
         , wDefaultConf_toHaTeX
         , fromHaTeX, toHaTeX, toHaTeX_wConfig
         , MathematicalLaTeX
         , MathematicalLaTeX_
         , MathematicalLaTeXT
         , MathematicalLaTeXT_
         , TeXMathStateProps(..)
         , texMathGroundState
         , tamperFreeVarStack
         , srcNLEnv
         ) where

import Math.LaTeX.Config

import Text.LaTeX.Base
import Text.LaTeX.Base.Class

import Math.LaTeX.TextMarkup

import Control.Applicative
import Control.Monad.Trans
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


newtype MathematicalLaTeXT freeVarStack baseMonad mRe
  = MathematicalLaTeXT { runMathematicalLaTeXT ::
     StateT TeXMathStateProps ( ReaderT TeXMathConfiguration
       (LaTeXT baseMonad) ) mRe }
type MathematicalLaTeXT_ m = MathematicalLaTeXT HNil' m ()
type MathematicalLaTeX f x = MathematicalLaTeXT f Identity x
type MathematicalLaTeX_ f = MathematicalLaTeXT f Identity ()

instance (Functor m) => Functor (MathematicalLaTeXT a m) where
  fmap f = MathematicalLaTeXT . fmap f . runMathematicalLaTeXT

instance (Monad m) => Monad (MathematicalLaTeXT a m) where
  return = MathematicalLaTeXT . return
  MathematicalLaTeXT stm >>= f
    = MathematicalLaTeXT $ stm >>= runMathematicalLaTeXT . f
  fail = MathematicalLaTeXT . fail

instance MonadTrans (MathematicalLaTeXT a) where
  lift = MathematicalLaTeXT . lift . lift . lift

instance (Monad m) => MonadReader TeXMathConfiguration (MathematicalLaTeXT f m) where
  ask = MathematicalLaTeXT ask
  local f (MathematicalLaTeXT q) = MathematicalLaTeXT $ local f q

instance (Monad m) => MonadState TeXMathStateProps (MathematicalLaTeXT f m) where
  get = MathematicalLaTeXT get
  put = MathematicalLaTeXT . put


instance (Monad m) => IsString (MathematicalLaTeXT f m a) where
  fromString s = do
     (TeXMathStateProps {..}) <- get
     mkupCfg <- askTextMarkupConfig
     fromHaTeX . fromLaTeX . (`runReader`mkupCfg) . markupTxtToLaTeX
       $ case punctuationNeededAtDisplayEnd of
          Just pnct -> pnct ++ " " ++ s
          Nothing   -> s

srcNLEnv :: LaTeX -> LaTeX
srcNLEnv e = raw"\n" <> e <> raw"\n"
                               
fromHaTeX :: Monad m => LaTeXT m a -> MathematicalLaTeXT f m a
fromHaTeX = MathematicalLaTeXT . lift.lift

toHaTeX :: Monad m => MathematicalLaTeXT f m a -> ReaderT TeXMathConfiguration (LaTeXT m) a
toHaTeX (MathematicalLaTeXT mLaTeX) = do
   cfg <- ask
   lift . (`runReaderT` cfg) . liftM fst $ runStateT mLaTeX texMathGroundState

toHaTeX_wConfig :: Monad m => TeXMathConfiguration -> MathematicalLaTeXT f m a -> LaTeXT m a
toHaTeX_wConfig cfg = (`runReaderT`cfg) . toHaTeX

wDefaultConf_toHaTeX :: Monad m => MathematicalLaTeXT f m a -> LaTeXT m a
wDefaultConf_toHaTeX = toHaTeX_wConfig mathLaTeXDefaultConfig


tamperFreeVarStack :: MathematicalLaTeXT f m a -> MathematicalLaTeXT f' m a
tamperFreeVarStack = MathematicalLaTeXT . runMathematicalLaTeXT


instance (Monad m) => HasTextMarkupConfig (MathematicalLaTeXT f m a) where
  modifyMarkupRules = local . modifyMarkupRules
