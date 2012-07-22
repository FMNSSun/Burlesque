module Burlesque.Display
  (Display (..))
 where

import Burlesque.Types

class Display a where
  toDisplay :: a -> String
  fromDisplay :: String -> a

instance Display BlsqExp where
  toDisplay (BlsqInt i) = show i
  toDisplay (BlsqStr s) = s
