module Burlesque.Eval
  (eval)
 where

import Burlesque.Types

eval :: BlsqProg -> BlsqState
eval (x:xs) = evalI x >> eval xs
eval [] = return ()

evalI v@(BlsqInt _) = modify (v:)
evalI v@(BlsqChar _) = modify (v:)
evalI v@(BlsqStr _) = modify (v:)
evalI v@(BlsqIdent i) = do
 case i of
   ".+" -> builtinAdd

evalI _ = return () 

builtinAdd :: BlsqState
builtinAdd = do
 st <- get
 case st of
   ((BlsqInt b):(BlsqInt a):xs) -> put $ (BlsqInt (a + b)) : xs
   ((BlsqStr b):(BlsqStr a):xs) -> put $ (BlsqStr (a ++ b)) : xs
   _ -> put $ (BlsqError "Berlesque: (.+) Invalid arguments!") : st
 
