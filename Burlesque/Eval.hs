module Burlesque.Eval
  (eval)
 where

import Burlesque.Types
import Burlesque.Builtins

eval :: BlsqProg -> BlsqState
eval (x:xs) = evalI x >> eval xs
eval [] = return ()



evalI v@(BlsqIdent i) = lookupBuiltin i
evalI v@(BlsqInt _) = modify (v:)
evalI v@(BlsqChar _) = modify (v:)
evalI v@(BlsqStr _) = modify (v:)
evalI _ = return () 
