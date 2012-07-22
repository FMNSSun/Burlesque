module Burlesque.Eval
  (eval)
 where

import Burlesque.Types

import Data.Maybe
import Data.List

eval :: BlsqProg -> BlsqState
eval (x:xs) = evalI x >> eval xs
eval [] = return ()

builtins = [
  (".+", builtinAdd),
  (".-", builtinSub),
  ("<-", builtinReverse),
  ("ln", builtinLines),
  ("ri", builtinReadInt)
 ]

lookupBuiltin b = fromMaybe (return ()) $ lookup b builtins

evalI v@(BlsqIdent i) = lookupBuiltin i
evalI v@(BlsqInt _) = modify (v:)
evalI v@(BlsqChar _) = modify (v:)
evalI v@(BlsqStr _) = modify (v:)
evalI _ = return () 

-- | builtinAdd
-- 
-- Int Int -> Regular integer addition
-- Str Str -> String concatenation
-- Int Str -> Take first n characters of a string
builtinAdd :: BlsqState
builtinAdd = do
 st <- get
 case st of
   ((BlsqInt b):(BlsqInt a):xs) -> put $ (BlsqInt (a + b)) : xs
   ((BlsqStr b):(BlsqStr a):xs) -> put $ (BlsqStr (a ++ b)) : xs
   ((BlsqStr b):(BlsqInt a):xs) -> put $ (BlsqStr $ take a b) : xs
   _ -> put $ (BlsqError "Burlesque: (.+) Invalid arguments!") : st

-- | builtinSub
--
-- Int Int -> Regular integer subtraction
-- Int Str -> Drop first n characters of a string
-- Str Str -> Opposite of string concatenation
builtinSub :: BlsqState
builtinSub = do
 st <- get
 case st of
   ((BlsqInt b):(BlsqInt a):xs) -> put $ (BlsqInt (a - b)) : xs
   ((BlsqStr b):(BlsqInt a):xs) -> put $ (BlsqStr $ drop a b) : xs
   ((BlsqStr b):(BlsqStr a):xs) -> if b `isSuffixOf` a
                                    then put $ (BlsqStr $ take (length a - length b) a) : xs
                                    else put $ (BlsqStr a) : xs
   _ -> put $ (BlsqError "Burlesque: (.-) Invalid arguments!") : st

-- | builtinReverse
-- Int -> Reverse digit
-- Str -> Reverse string
builtinReverse :: BlsqState
builtinReverse = do
 st <- get
 case st of
  (BlsqStr a) : xs -> put $ (BlsqStr $ reverse a) : xs
  (BlsqInt a) : xs -> put $ (BlsqInt . read . reverse . show $ a) : xs
  _ -> put $ (BlsqError "Burlesque: (<-) Invalid arguments!") : st

-- | builtinLines
-- Str -> Returns a list of lines
-- Int -> Number of digits
builtinLines :: BlsqState
builtinLines = do
 st <- get
 case st of
  (BlsqStr a) : xs -> put $ (BlsqBlock . map BlsqStr . lines $ a) : xs
  (BlsqInt a) : xs -> put $ (BlsqInt . length . show $ a) : xs
  _ -> put $ (BlsqError "Burlesque: (ln) Invalid arguments!") : st

-- | builtinReadInt
-- Int -> Identity
-- Str -> Convert to Int
builtinReadInt :: BlsqState
builtinReadInt = do
 st <- get
 case st of
  (BlsqStr a) : xs -> put $ (BlsqInt . read $ a) : xs
  (BlsqInt a) : xs -> put $ (BlsqInt a) : xs
  _ -> put $ (BlsqError "Burlesque: (ri) Invalid arguments!") : st
