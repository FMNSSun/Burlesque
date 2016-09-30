{-# LANGUAGE FlexibleContexts #-}

module Burlesque.Types 
  (BlsqExp (..),
   BlsqStack (..),
   BlsqState (..),
   BlsqState' (..),
   BlsqProg (..),
   BlsqPrettyFormat (..),
   module Control.Monad.State.Lazy)
 where

import Control.Monad.State.Lazy
import Data.Map as M

data BlsqExp =  BlsqInt Integer
              | BlsqDouble Double
              | BlsqChar Char
              | BlsqStr String
              | BlsqIdent String
              | BlsqSpecial String
              | BlsqBlock [BlsqExp]
              | BlsqError String
              | BlsqNil
              | BlsqPretty BlsqExp BlsqPrettyFormat
              | BlsqQuoted [BlsqExp]
              | BlsqHackMode String
              | BlsqHiddenState BlsqExp
              | BlsqMapBlock [BlsqExp]
              | BlsqFilterBlock [BlsqExp]
              | BlsqReduceBlock [BlsqExp]
              | BlsqAssign String BlsqExp Bool Bool
              | BlsqCall String
              | BlsqGet String
              | BlsqMap (M.Map BlsqExp BlsqExp) BlsqExp
  deriving (Show,Eq,Read,Ord)

data BlsqPrettyFormat =  BlsqFormatNormal
                       | BlsqFormatNoSpaces
                       | BlsqFormatWithSpaces
                       | BlsqFormatRaw
  deriving (Show,Read,Eq,Ord)

type BlsqStack = [BlsqExp]
type BlsqState = StateT (BlsqStack, BlsqStack, M.Map BlsqExp BlsqExp) IO ()
type BlsqState' a = StateT (BlsqStack, BlsqStack, M.Map BlsqExp BlsqExp) IO a
type BlsqProg = [BlsqExp]
