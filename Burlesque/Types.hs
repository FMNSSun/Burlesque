module Burlesque.Types 
  (BlsqExp (..),
   BlsqStack (..),
   BlsqState (..),
   BlsqProg (..),
   module Control.Monad.State)
 where

import Control.Monad.State

data BlsqExp =  BlsqInt Int 
              | BlsqChar Char
              | BlsqStr String
              | BlsqIdent String
              | BlsqSpecial String
              | BlsqBlock [BlsqExp]
              | BlsqError String
  deriving (Show,Eq,Read)

type BlsqStack = [BlsqExp]
type BlsqState = State BlsqStack ()
type BlsqProg = [BlsqExp]
