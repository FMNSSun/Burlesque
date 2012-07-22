module Burlesque.Display
  (Display (..))
 where

import Burlesque.Types

import Data.List

class Display a where
  toDisplay :: a -> String
  fromDisplay :: String -> a

instance Display BlsqExp where
  toDisplay (BlsqInt i) = show i
  toDisplay (BlsqBlock xs) = "[" ++ (intercalate "," $ map toDisplay xs) ++ "]"
  toDisplay (BlsqStr s) = show s
  toDisplay (BlsqError s) = "ERROR: " ++ s
  toDisplay (BlsqIdent s) = "<" ++ show s ++ ">"
  toDisplay (BlsqChar c) = show c
  toDisplay q = show q
