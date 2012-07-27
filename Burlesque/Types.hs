module Burlesque.Types 
  (BlsqExp (..),
   BlsqStack (..),
   BlsqState (..),
   BlsqProg (..),
   BlsqPrettyFormat (..),
   module Control.Monad.State)
 where

import Control.Monad.State

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
  deriving (Show,Eq,Read,Ord)

data BlsqPrettyFormat =  BlsqFormatNormal
                       | BlsqFormatNoSpaces
                       | BlsqFormatWithSpaces
  deriving (Show,Read,Eq,Ord)

type BlsqStack = [BlsqExp]
type BlsqState = State BlsqStack ()
type BlsqProg = [BlsqExp]
