module Z.Types 
  (ZExp (..),
   ZStack (..),
   ZState (..),
   ZProg (..),
   ZPrettyFormat (..),
   module Control.Monad.State)
 where

import Control.Monad.State

data ZExp =  ZInt Integer
              | ZDouble Double
              | ZChar Char
              | ZStr String
              | ZIdent String
              | ZSpecial String
              | ZBlock [ZExp]
              | ZError String
              | ZNil
              | ZQuoted ZExp

  deriving (Show,Eq,Read,Ord)

data ZPrettyFormat =  ZFormatNormal
                       | ZFormatNoSpaces
                       | ZFormatWithSpaces
                       | ZFormatRaw
  deriving (Show,Read,Eq,Ord)

type ZStack = [ZExp]
type ZState = StateT ZStack IO ()
type ZProg = [ZExp]
