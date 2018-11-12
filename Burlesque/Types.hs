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
import Control.Concurrent.Chan

instance Show (Chan a) where
  show _ = "_CHAN_"
  
instance Ord (Chan a) where
  _ <= _ = True

instance Read (Chan a) where
  readsPrec _ _ = [(error "fuck you you can't read chans", "")]

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
              | BlsqCall String Bool
              | BlsqGet String
              | BlsqMap (M.Map BlsqExp BlsqExp) BlsqExp
              | BlsqSet String BlsqExp
              | BlsqAutoBlock [BlsqExp]
              | BlsqChan (Chan BlsqExp)
              | BlsqSExp BlsqExp [BlsqExp]
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
