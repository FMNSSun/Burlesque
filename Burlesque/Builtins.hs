module Burlesque.Builtins
  (builtins,
   lookupBuiltin)
 where

import Burlesque.Types

import Data.Maybe
import Data.List

builtins = [
  (".+", builtinAdd),
  (".-", builtinSub),
  ("<-", builtinReverse),
  ("ln", builtinLines),
  ("ri", builtinReadInt)
 ]

lookupBuiltin b = fromMaybe (return ()) $ lookup b builtins

-- | builtinAdd
-- 
-- Int Int -> Regular integer addition
-- Str Str -> String concatenation
-- Int Str -> Take first n characters of a string
builtinAdd :: BlsqState
builtinAdd = do
 st <- get
 put $
  case st of
    ((BlsqInt b):(BlsqInt a):xs) -> (BlsqInt (a + b)) : xs
    ((BlsqStr b):(BlsqStr a):xs) -> (BlsqStr (a ++ b)) : xs
    ((BlsqStr b):(BlsqInt a):xs) -> (BlsqStr $ take a b) : xs
    _ -> (BlsqError "Burlesque: (.+) Invalid arguments!") : st

-- | builtinSub
--
-- Int Int -> Regular integer subtraction
-- Int Str -> Drop first n characters of a string
-- Str Str -> Opposite of string concatenation
builtinSub :: BlsqState
builtinSub = do
 st <- get
 put $
  case st of
    ((BlsqInt b):(BlsqInt a):xs) -> (BlsqInt (a - b)) : xs
    ((BlsqStr b):(BlsqInt a):xs) -> (BlsqStr $ drop a b) : xs
    ((BlsqStr b):(BlsqStr a):xs) -> if b `isSuffixOf` a
                                     then (BlsqStr $ take (length a - length b) a) : xs
                                     else (BlsqStr a) : xs
    _ -> (BlsqError "Burlesque: (.-) Invalid arguments!") : st

-- | builtinReverse
-- Int -> Reverse digit
-- Str -> Reverse string
builtinReverse :: BlsqState
builtinReverse = do
 st <- get
 put $
  case st of
   (BlsqStr a) : xs -> (BlsqStr $ reverse a) : xs
   (BlsqInt a) : xs -> (BlsqInt . read . reverse . show $ a) : xs
   _ -> (BlsqError "Burlesque: (<-) Invalid arguments!") : st

-- | builtinLines
-- Str -> Returns a list of lines
-- Int -> Number of digits
builtinLines :: BlsqState
builtinLines = do
 st <- get
 put $
  case st of
   (BlsqStr a) : xs -> (BlsqBlock . map BlsqStr . lines $ a) : xs
   (BlsqInt a) : xs -> (BlsqInt . length . show $ a) : xs
   _ -> (BlsqError "Burlesque: (ln) Invalid arguments!") : st

-- | builtinReadInt
-- Int -> Identity
-- Str -> Convert to Int
builtinReadInt :: BlsqState
builtinReadInt = do
 st <- get
 put $
  case st of
   (BlsqStr a) : xs -> (BlsqInt . read $ a) : xs
   (BlsqInt a) : xs -> (BlsqInt a) : xs
   _ -> (BlsqError "Burlesque: (ri) Invalid arguments!") : st
