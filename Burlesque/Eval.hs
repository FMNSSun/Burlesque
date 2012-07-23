module Burlesque.Eval
  (eval, run, runStack)
 where

import Burlesque.Types
import Burlesque.Parser

import Data.Maybe
import Data.List

import Debug.Trace

-- | Evaluate a Burlesque program
eval :: BlsqProg -> BlsqState
eval (x:xs) = evalI x >> eval xs
eval [] = return ()

evalI v@(BlsqIdent i) = lookupBuiltin i
evalI v = modify (v:)

-- | Run program with empty stack
run :: BlsqProg -> BlsqStack
run p = runStack p []

-- | Run program with predefined stack
runStack :: BlsqProg -> BlsqStack -> BlsqStack
runStack p xs = execState (eval p) xs

builtins = [
  (".+", builtinAdd),
  (".-", builtinSub),
  ("<-", builtinReverse),
  ("ln", builtinLines),
  ("ri", builtinReadInt),
  ("ps", builtinParse),
  ("if", builtinIff),
  ("e!", builtinEval),
  ("w!", builtinWhile),
  ("++", builtinSum),
  ("[~", builtinLast),
  ("~]", builtinInit),
  ("\\[", builtinConcat),
  ("m[", builtinMap),
  ("\\/", builtinSwap),
  ("^^", builtinDup),
  ("vv", builtinPop)
 ]

lookupBuiltin b = fromMaybe (return ()) $ lookup b builtins

putResult = put

-- | builtinAdd
-- 
-- Int Int -> Regular integer addition
-- Str Str -> String concatenation
-- Int Str -> Take first n characters of a string
builtinAdd :: BlsqState
builtinAdd = do
 st <- get
 putResult $
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
 putResult $
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
 putResult $
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
 putResult $
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
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqInt . read $ a) : xs
   (BlsqInt a) : xs -> (BlsqInt a) : xs
   _ -> (BlsqError "Burlesque: (ri) Invalid arguments!") : st

-- | builtinParse
-- Str -> Parses a string as a BlsqExp
builtinParse :: BlsqState
builtinParse = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqBlock (runParserWithString parseBlsq a)) : xs
   _ -> (BlsqError "Burlesque: (ps) Invalid arguments!") : st

-- | builtinSum
-- Block -> Sum of all (Int) elements
builtinSum :: BlsqState
builtinSum = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> (sum' a) : xs
   _ -> (BlsqError "Burlesque: (++) Invalid arguments!") : st
 where sum' [] = BlsqInt 0
       sum' (BlsqInt a : xs) = 
         case sum' xs of
           BlsqInt b -> BlsqInt (a + b)
           q -> q
       sum' _ = BlsqError "Burlesque: (++) Invalid element!" 

-- | builtinLast
-- Block -> Last element
-- Str -> Last character
builtinLast :: BlsqState
builtinLast = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> last a : xs
   (BlsqStr a) : xs -> BlsqChar (last a) : xs
   _ -> (BlsqError "Burlesque: ([~) Invalid arguments!") : st

-- | builtinInit
-- Block -> All except last elements
-- Str -> All except last character
builtinInit :: BlsqState
builtinInit = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> (BlsqBlock (init a)) : xs
   (BlsqStr a) : xs -> BlsqStr (init a) : xs
   _ -> (BlsqError "Burlesque: (~]) Invalid arguments!") : st

-- | builtinConcat
-- Block -> Concatenates Blocks in Block or Strings in Block
builtinConcat :: BlsqState
builtinConcat = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> (concat' a) : xs
   _ -> (BlsqError "Burlesque: (\\[) Invalid arguments!") : st
 where concat' [] = BlsqNil
       concat' (BlsqBlock a : as) = 
         case concat' as of
           BlsqBlock b -> BlsqBlock $ a ++ b
           BlsqError q -> BlsqError q
           BlsqNil -> BlsqBlock $ a
           _ -> BlsqError "Burlesque: (\\[) Invalid element! Expecting Block!" 
       concat' (BlsqStr a : as) =
         case concat' as of
           BlsqStr b -> BlsqStr $ a ++ b
           BlsqError q -> BlsqError q
           BlsqNil -> BlsqStr $ a
           _ -> BlsqError "Burlesque: (\\[) Invalid element! Expecting String!"
       concat' _ = BlsqError "Burlesque: (\\[) Invalid element!"

-- | builtinMap
-- Block Block -> Map
builtinMap :: BlsqState
builtinMap = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> (BlsqBlock $ map' a b) : xs
   _ -> BlsqError "Burlesque: (m[) Invalid arguments!" : st
 where map' f [] = []
       map' f (x:xs) = (runStack f [x]) ++ (map' f xs)

-- | builtinSwap
-- StackManip
builtinSwap :: BlsqState
builtinSwap = do
 st <- get
 putResult $
  case st of
   (a : b : xs) -> b : a : xs
   _ -> BlsqError "Burlesque: (\\/) Stack size error!" : st

-- | builtinDup
-- StackManip
builtinDup :: BlsqState
builtinDup = do
 st <- get
 putResult $
  case st of
   (a : xs) -> (a : a : xs)
   _ -> BlsqError "Burlesque: (^^) Stack size error!" : st

-- | builtinPop
-- StackManip
builtinPop :: BlsqState
builtinPop = do
 st <- get
 putResult $
  case st of
   (a : xs) -> xs
   _ -> BlsqError "Burlesque: (vv) Stack size error!" : st

-- | builtinIff
-- Block Int -> If and only if
builtinIff :: BlsqState
builtinIff = do
 st <- get
 putResult $
  case st of
   (BlsqInt 0 : BlsqBlock b : xs) -> xs
   (BlsqInt _ : BlsqBlock b : xs) -> runStack b xs
   _ -> BlsqError "Burlesque (if) Invalid arguments!" : st

-- | eval
-- Block -> Eval
builtinEval :: BlsqState
builtinEval = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : xs) -> runStack b xs
   _ -> BlsqError "Burlesque (e!) Invalid arguments!" : st

builtinWhile :: BlsqState
builtinWhile = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> while' a b xs
   _ -> BlsqError "Burlesque (w!) Invalid arguments!" : st
 where while' f g xs = case runStack g xs of
                        (BlsqInt 0 : ys) -> xs
                        (BlsqInt a : ys) -> while' f g $ runStack f xs
                        (_ : ys) -> BlsqError "Burlesque (w!) Invalid!" : ys
                        _ -> BlsqError "Burlesque (w!) Stack size error!" : xs
