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
  (".-", builtinSub)
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
   _ -> put $ (BlsqError "Berlesque: (.+) Invalid arguments!") : st

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
   _ -> put $ (BlsqError "Berlesque: (.-) Invalid arguments!") : st
 
