module Z.Eval
  (eval, run, runStack, builtins)
 where

import Z.Types
import Z.Parser
import Z.Helpers
import Z.Display

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char
import Data.Bits
import Data.Ord
import Text.Regex
import Numeric
import Control.Monad
import System.Random

import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Distribution.Binomial
import Statistics.Distribution.Poisson
import Statistics.Distribution.Geometric
import Statistics.Distribution.Hypergeometric
import Statistics.Distribution.ChiSquared
import Statistics.Distribution.Exponential
import Statistics.Distribution.StudentT
import Statistics.Distribution.Uniform

import Debug.Trace

-- | > Evaluate a Z program
eval :: ZProg -> ZState
--eval (ZSpecial ")" : i : xs) = do
-- modify (ZBlock [ i ] : )
-- builtinMap
-- eval xs
 
eval (x:xs) = evalI x >> eval xs
eval [] = return ()

evalI (ZQuoted q) = modify (q:)
evalI v@(ZSpecial ",") = do
 st <- get
 if length st == 1 then
   do put []
 else return ()
evalI v@(ZIdent i) = lookupBuiltin i
evalI v = modify (v:)

-- | > Run program with empty stack
run :: ZProg -> IO ZStack
run p = runStack p []

-- | > Run program with predefined stack
runStack :: ZProg -> ZStack -> IO ZStack
runStack p xs = execStateT (eval p) xs

toInt p = (fromIntegral p) :: Int

builtins = [
  (".+", builtinAdd),
  ("m[", builtinMap),
  ("vv", builtinPop)
 ]

lookupBuiltin b = fromMaybe (modify (ZError ("Unknown command: (" ++ b ++ ")!") :)) $ lookup b builtins

putResult = put

builtinAdd :: ZState
builtinAdd = do
 st <- get
 case st of
   (ZInt b : ZInt a : xs) -> putResult $ ZInt (a + b) : xs
   (ZDouble b : ZDouble a : xs) -> putResult $ ZDouble (a + b) : xs
   (ZStr b : ZStr a : xs) -> putResult $ ZStr (a ++ b) : xs
   
builtinMap :: ZState
builtinMap = do
 st <- get
 case st of
   (ZBlock a : ZBlock b : xs) -> do
      q <- liftIO $ map' a b
      putResult $ (ZBlock $ q) : xs
   _ -> putResult $ ZError "(m[) Invalid arguments!" : st
 where map' _ [] = return []
       map' f (x:xs) = do
         a <- runStack f [x]
         b <- map' f xs
         return $ a ++ b

builtinPop :: ZState
builtinPop = do
 st <- get
 putResult $
  case st of
   (a : xs) -> xs
   _ -> ZError "(vv) Stack size error!" : st
