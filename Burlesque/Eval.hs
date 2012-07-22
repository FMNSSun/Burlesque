module Burlesque.Eval
  (eval)
 where

import Burlesque.Types
import Burlesque.Builtins

eval :: BlsqProg -> BlsqState
eval (x:xs) = evalI x >> eval xs
eval [] = return ()



evalI v@(BlsqIdent i) = lookupBuiltin i
evalI v = modify (v:)
