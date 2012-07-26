module Burlesque.Eval
  (eval, run, runStack, builtins)
 where

import Burlesque.Types
import Burlesque.Parser

import Data.Maybe
import Data.List
import Data.Char

import Debug.Trace

-- | > Evaluate a Burlesque program
eval :: BlsqProg -> BlsqState
eval (x:xs) = evalI x >> eval xs
eval [] = return ()

evalI v@(BlsqIdent i) = lookupBuiltin i
evalI v = modify (v:)

-- | > Run program with empty stack
run :: BlsqProg -> BlsqStack
run p = runStack p []

-- | > Run program with predefined stack
runStack :: BlsqProg -> BlsqStack -> BlsqStack
runStack p xs = execState (eval p) xs

toInt p = (fromIntegral p) :: Int

builtins = [
  (".+", builtinAdd),
  (".-", builtinSub),
  ("./", builtinDiv),
  (".*", builtinMul),
  ("+.", builtinIncrement),
  ("-.", builtinDecrement),
  (".>", builtinGreater),
  (".<", builtinSmaller),
  (">.", builtinMax),
  ("<.", builtinMin),
  (">]", builtinMaximum),
  ("<]", builtinMinimum),
  ("**", builtinPow),
  ("r_", builtinRound),
  ("==", builtinEqual),
  ("<-", builtinReverse),
  ("ln", builtinLines),
  ("un", builtinUnlines),
  ("wl", builtinWithLines),
  ("ri", builtinReadInt),
  ("ps", builtinParse),
  ("if", builtinIff),
  ("ie", builtinIfElse),
  ("e!", builtinEval),
  ("c!", builtinContinuation),
  ("w!", builtinWhile),
  ("++", builtinSum),
  ("[~", builtinLast),
  ("~]", builtinInit),
  ("[-", builtinTail),
  ("-]", builtinHead),
  ("[+", builtinAppend),
  ("\\[", builtinConcat),
  ("[[", builtinIntersperse),
  ("m[", builtinMap),
  ("r[", builtinReduce),
  ("\\/", builtinSwap),
  ("^^", builtinDup),
  ("vv", builtinPop),
  ("XX", builtinExplode)
 ]

lookupBuiltin b = fromMaybe (return ()) $ lookup b builtins

putResult = put

-- | > .+
-- 
-- > Int Int -> Regular integer addition
-- > Double Double -> Addition
-- > Str Str -> String concatenation
-- > Block Block -> Block concatenation
-- > Int Str -> Take first n characters of a string
builtinAdd :: BlsqState
builtinAdd = do
 st <- get
 putResult $
  case st of
    ((BlsqInt b):(BlsqInt a):xs) -> (BlsqInt (a + b)) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a + b)) : xs
    ((BlsqStr b):(BlsqStr a):xs) -> (BlsqStr (a ++ b)) : xs
    ((BlsqStr b):(BlsqInt a):xs) -> (BlsqStr $ genericTake a b) : xs
    (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (a ++ b) : xs
    (BlsqChar b : BlsqChar a : xs) -> (BlsqStr $ a:b:"") : xs
    (BlsqChar b : BlsqStr a : xs) -> (BlsqStr $ a++[b]) : xs
    _ -> (BlsqError $ "Burlesque: (.+) Invalid arguments!") : st

-- | > .-
--
-- > Int Int -> Regular integer subtraction
-- > Double Double -> Subtraction
-- > Int Str -> Drop first n characters of a string
-- > Str Str -> Opposite of string concatenation
builtinSub :: BlsqState
builtinSub = do
 st <- get
 putResult $
  case st of
    ((BlsqInt b):(BlsqInt a):xs) -> (BlsqInt (a - b)) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a - b)) : xs
    ((BlsqStr b):(BlsqInt a):xs) -> (BlsqStr $ genericDrop a b) : xs
    ((BlsqStr b):(BlsqStr a):xs) -> if b `isSuffixOf` a
                                     then (BlsqStr $ genericTake (genericLength a - length b) a) : xs
                                     else (BlsqStr a) : xs
    _ -> (BlsqError "Burlesque: (.-) Invalid arguments!") : st

-- | > .*
--
-- > Int Int -> Regular integer multiplication
-- > Double Double -> Multiplication
-- > Str Int -> List containing n copies of Str
-- > Char Int -> A string containing n copies of Char
-- > Block Int -> A Block containing n copies of Block
builtinMul :: BlsqState
builtinMul = do
 st <- get
 putResult $
  case st of
    (BlsqInt b : BlsqInt a : xs) -> BlsqInt (a * b) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a * b)) : xs
    (BlsqInt b : BlsqStr a : xs) -> BlsqBlock (genericReplicate b (BlsqStr a)) : xs
    (BlsqInt b : BlsqChar a : xs) -> BlsqStr (genericReplicate b a) : xs
    (BlsqInt b : BlsqBlock a : xs) -> BlsqBlock (genericReplicate b (BlsqBlock a)) : xs
    _ -> (BlsqError "Burlesque: (.*) Invalid arguments!") : st

-- | > ./
--
-- > Int Int -> Regular integer multiplication
-- > Double Double -> Division
builtinDiv :: BlsqState
builtinDiv = do
 st <- get
 putResult $
  case st of
    (BlsqInt b : BlsqInt a : xs) -> BlsqInt (a `div` b) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a / b)) : xs
    _ -> (BlsqError "Burlesque: (./) Invalid arguments!") : st

-- | > +.
--
-- Int -> Increment
-- Char -> Next character
-- String -> Duplicate last character
-- Block -> Duplicate last element
builtinIncrement :: BlsqState
builtinIncrement = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : xs) -> BlsqInt (a+1) : xs
   (BlsqChar a : xs) -> BlsqChar (chr $ ord a + 1) : xs
   (BlsqStr a : xs) -> BlsqStr (a ++ [last a]) : xs
   (BlsqBlock a : xs) -> BlsqBlock (a ++ [last a]) : xs
   _ -> (BlsqError "Burlesque: (+.) Invalid arguments!") : st

-- | > -.
--
-- Int -> Decrement
-- Char -> Previous character
-- String -> Duplicate first character
-- Block -> Duplicate first element
builtinDecrement :: BlsqState
builtinDecrement = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : xs) -> BlsqInt (a-1) : xs
   (BlsqChar a : xs) -> BlsqChar (chr $ ord a - 1) : xs
   (BlsqStr a : xs) -> BlsqStr (head a : a) : xs
   (BlsqBlock a : xs) -> BlsqBlock (head a : a) : xs
   _ -> (BlsqError "Burlesque: (-.) Invalid arguments!") : st
   
   

-- | > **
--
-- > Int Int -> math pow
-- > Double Double -> math pow
-- > Block Block -> Merge blocks
-- > Str Str -> Merge strings
-- > Char -> ord
builtinPow :: BlsqState
builtinPow = do
 st <- get
 putResult $
  case st of
    (BlsqInt b : BlsqInt a : xs) -> BlsqInt (a ^ b) : xs
    (BlsqDouble b : BlsqDouble a : xs) -> BlsqDouble (a ** b) : xs
    (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (merge' a b) : xs
    (BlsqStr b : BlsqStr a : xs) -> BlsqStr (merge' a b) : xs
    (BlsqChar a : xs) -> BlsqInt (toInteger . ord $ a) : xs
    _ -> (BlsqError "Burlesque: (**) Invalid arguments!") : st
 where merge' (x:xs) [] = x:xs
       merge' [] (y:ys) = y:ys
       merge' [] [] = []
       merge' (x:xs) (y:ys) = x : y : merge' xs ys

  

-- | > <-
--
-- > Int -> Reverse digit
-- > Str -> Reverse string
-- > Block -> Reverse block
builtinReverse :: BlsqState
builtinReverse = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqStr $ reverse a) : xs
   (BlsqInt a) : xs -> (BlsqInt . read . reverse . show $ a) : xs
   (BlsqBlock a) : xs -> (BlsqBlock $ reverse a) : xs
   _ -> (BlsqError "Burlesque: (<-) Invalid arguments!") : st

-- | > ln
--
-- > Str -> Returns a list of lines
-- > Int -> Number of digits
builtinLines :: BlsqState
builtinLines = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqBlock . map BlsqStr . lines $ a) : xs
   (BlsqInt a) : xs -> (BlsqInt . genericLength . show $ a) : xs
   _ -> (BlsqError "Burlesque: (ln) Invalid arguments!") : st

-- | > un
--
-- ??
builtinUnlines :: BlsqState
builtinUnlines = do
 modify (BlsqStr "\n" :)
 builtinSwap
 builtinIntersperse
 builtinConcat

-- | > [[
--
-- > Any, Block -> Intersperse
builtinIntersperse :: BlsqState
builtinIntersperse = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : a : xs) -> (BlsqBlock $ intersperse a b) : xs
   _ -> (BlsqError "Burlesque: ([[) Invalid arguments!") : st

-- | > ri
--
-- > Int -> Identity
-- > Str -> Convert to Int
builtinReadInt :: BlsqState
builtinReadInt = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqInt . read $ a) : xs
   (BlsqInt a) : xs -> (BlsqInt a) : xs
   _ -> (BlsqError "Burlesque: (ri) Invalid arguments!") : st

-- | > ps
--
-- > Str -> Parses a string as a BlsqExp
builtinParse :: BlsqState
builtinParse = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqBlock (runParserWithString parseBlsq a)) : xs
   _ -> (BlsqError "Burlesque: (ps) Invalid arguments!") : st

-- | > ++
--
-- > Block -> Sum of all (Int) elements
-- > Int , Int -> Concatenate digits
builtinSum :: BlsqState
builtinSum = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> (sum' a) : xs
   (BlsqInt b : BlsqInt a : xs) -> (BlsqInt . read $ (show (abs a)) ++ (show (abs b))) : xs
   _ -> (BlsqError "Burlesque: (++) Invalid arguments!") : st
 where sum' [] = BlsqInt 0
       sum' (BlsqInt a : xs) = 
         case sum' xs of
           BlsqInt b -> BlsqInt (a + b)
           q -> q
       sum' _ = BlsqError "Burlesque: (++) Invalid element!" 

-- | > [~
--
-- > Block -> Last element
-- > Str -> Last character
builtinLast :: BlsqState
builtinLast = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> last a : xs
   (BlsqStr a) : xs -> BlsqChar (last a) : xs
   _ -> (BlsqError "Burlesque: ([~) Invalid arguments!") : st

-- | > ~]
--
-- > Block -> All except last elements
-- > Str -> All except last character
builtinInit :: BlsqState
builtinInit = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> (BlsqBlock (init a)) : xs
   (BlsqStr a) : xs -> BlsqStr (init a) : xs
   _ -> (BlsqError "Burlesque: (~]) Invalid arguments!") : st

-- | > [-
--
-- > Block -> All except first element
-- > Str -> All except first character
builtinTail :: BlsqState
builtinTail = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> (BlsqBlock (tail a)) : xs
   (BlsqStr a) : xs -> BlsqStr (tail a) : xs
   _ -> (BlsqError "Burlesque: ([-) Invalid arguments!") : st

-- | > -]
--
-- > Block -> First element
-- > Str -> First character
builtinHead :: BlsqState
builtinHead = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> head a : xs
   (BlsqStr a) : xs -> BlsqChar (head a) : xs
   _ -> (BlsqError "Burlesque: (-]) Invalid arguments!") : st

-- | > [+
--
-- > Block Any -> Append element
-- > Str Char -> Append char
builtinAppend :: BlsqState
builtinAppend = do
 st <- get
 putResult $
  case st of
   (b : BlsqBlock a : xs) -> BlsqBlock (a ++ [b]) : xs
   (BlsqChar b : BlsqStr a : xs) -> BlsqStr (a ++ [b]) : xs
   _ -> (BlsqError "Burlesque: ([+) Invalid arguments!") : st

-- | > \[
--
-- > Block -> Concatenates
builtinConcat :: BlsqState
builtinConcat = do
 st <- get
 case st of
  (BlsqBlock a) : xs -> do
     modify ((BlsqBlock $ [BlsqIdent ".+"]) :)
     builtinReduce
  _ -> putResult $ (BlsqError "Burlesque: (\\[) Invalid arguments!") : st

-- | > m[
--
-- > Block Block -> Map
builtinMap :: BlsqState
builtinMap = do
 st <- get
 case st of
   (BlsqBlock v : BlsqStr f : xs) -> do
      builtinSwap
      builtinExplode
      builtinSwap
      builtinMap
      builtinConcat
   (BlsqBlock a : BlsqBlock b : xs) -> putResult $ (BlsqBlock $ map' a b) : xs
   _ -> putResult $ BlsqError "Burlesque: (m[) Invalid arguments!" : st
 where map' _ [] = []
       map' f (x:xs) = (runStack f [x]) ++ (map' f xs)

builtinReduce :: BlsqState
builtinReduce = do
 st <- get
 putResult $
  case st of
   (BlsqBlock f : BlsqBlock ls : xs) -> (reduce' f ls) : xs
   _ -> BlsqError "Burlesque: (r[) Invalid arguments!" : st
 where reduce' f [] = BlsqError "Burlesque: (r[) Empty list!"
       reduce' f (x:xs) = reduce'' f x xs
       reduce'' f z [] = z
       reduce'' f z (x:xs) = case runStack f [x,z] of
                              (a : ys) -> reduce'' f a xs
                              _ -> BlsqError "Burlesque: (r[) Stack size error!"

-- | > wl
--
-- ??
builtinWithLines :: BlsqState
builtinWithLines = do
 builtinSwap
 builtinLines
 builtinSwap
 builtinMap
 builtinUnlines

-- | > \/
--
-- > StackManip -> Swap
builtinSwap :: BlsqState
builtinSwap = do
 st <- get
 putResult $
  case st of
   (a : b : xs) -> b : a : xs
   _ -> BlsqError "Burlesque: (\\/) Stack size error!" : st

-- | > ^^
--
-- > StackManip -> Duplicate
builtinDup :: BlsqState
builtinDup = do
 st <- get
 putResult $
  case st of
   (a : xs) -> (a : a : xs)
   _ -> BlsqError "Burlesque: (^^) Stack size error!" : st

-- | > vv
--
-- > StackManip -> Pop
builtinPop :: BlsqState
builtinPop = do
 st <- get
 putResult $
  case st of
   (a : xs) -> xs
   _ -> BlsqError "Burlesque: (vv) Stack size error!" : st

-- | > if
--
-- > Block Int -> If and only if
builtinIff :: BlsqState
builtinIff = do
 st <- get
 putResult $
  case st of
   (BlsqInt 0 : BlsqBlock b : xs) -> xs
   (BlsqInt _ : BlsqBlock b : xs) -> runStack b xs
   _ -> BlsqError "Burlesque: (if) Invalid arguments!" : st

-- | > ie
--
-- > Block a Block b Int -> If then a else b
builtinIfElse:: BlsqState
builtinIfElse = do
 st <- get
 putResult $
  case st of
   (BlsqInt 0 : BlsqBlock b : BlsqBlock a : xs) -> runStack b xs
   (BlsqInt _ : BlsqBlock b : BlsqBlock a : xs) -> runStack a xs
   _ -> BlsqError "Burlesque: (ie Invalid arguments!" : st

-- | > e!
--
-- > Block -> Eval
builtinEval :: BlsqState
builtinEval = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : xs) -> runStack b xs
   _ -> BlsqError "Burlesque: (e!) Invalid arguments!" : st

-- | > c!
--
-- > Block -> Continuation
builtinContinuation :: BlsqState
builtinContinuation = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : xs) -> case runStack b xs of
                           (a:ys) -> a : xs
                           _ -> st
   _ -> BlsqError "Burlesque: (c!) Invalid arguments!" : st

-- | > w!
--
-- > Block f, Block g -> while g do f
builtinWhile :: BlsqState
builtinWhile = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> while' a b xs
   (BlsqBlock a : xs) -> while' a [] xs
   _ -> BlsqError "Burlesque: (w!) Invalid arguments!" : st
 where while' f g xs = case runStack g xs of
                        (BlsqInt 0 : ys) -> xs
                        (BlsqInt a : ys) -> while' f g $ runStack f xs
                        (_ : ys) -> BlsqError "Burlesque: (w!) Invalid!" : ys
                        _ -> BlsqError "Burlesque: (w!) Stack size error!" : xs

-- | > (.>)
--
-- > Int a , Int b -> a > b 
-- > Double a, Double b -> a > b
builtinGreater :: BlsqState
builtinGreater = do
 st <- get
 putResult $
  case st of
   (BlsqInt b : BlsqInt a : xs) -> (BlsqInt $ if a > b then 1 else 0) : xs
   (BlsqDouble b : BlsqDouble a : xs) -> (BlsqInt $ if a > b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (.>) Invalid arguments!" : st

-- | > (.<)
--
-- > Int a , Int b -> a < b 
-- > Double a, Double b -> a < b
builtinSmaller :: BlsqState
builtinSmaller = do
 st <- get
 putResult $
  case st of
   (BlsqInt b : BlsqInt a : xs) -> (BlsqInt $ if a < b then 1 else 0) : xs
   (BlsqDouble b : BlsqDouble a : xs) -> (BlsqInt $ if a < b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (.<) Invalid arguments!" : st

-- | > (>.)
--
-- > Any a, Any b -> Max(a,b)
--
builtinMax :: BlsqState
builtinMax = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (max a b) : xs
   _ -> BlsqError "Burlesque: (>.) Stack size error!" : st

-- | > (<.)
--
-- > Any a, Any b -> Min(a,b)
builtinMin :: BlsqState
builtinMin = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (min a b) : xs
   _ -> BlsqError "Burlesque: (<.) Stack size error!" : st

-- | > (>])
--
-- > Block a-> Maximum(a)
--
builtinMaximum :: BlsqState
builtinMaximum = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : xs) -> (maximum a) : xs
   _ -> BlsqError "Burlesque: (>]) Invalid arguments!" : st

-- | > (<])
--
-- > Block a -> Minimum(a)
builtinMinimum :: BlsqState
builtinMinimum = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : xs) -> (minimum a) : xs
   _ -> BlsqError "Burlesque: (<]) Invalid arguments!" : st


-- | > (==)
--
-- > Any a , Any b -> a == b 
builtinEqual :: BlsqState
builtinEqual = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a == b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (==) Invalid arguments!" : st

-- | > (r_)
--
-- > Double a, Int b -> Round a to b decimal places
builtinRound :: BlsqState
builtinRound = do
 st <- get
 putResult $
  case st of
   (BlsqInt b : BlsqDouble a : xs) -> (BlsqDouble $ round' a b) : xs
   _ -> BlsqError "Burlesque: (r_) Invalid arguments!" : st
 where round' n s = let factor = fromIntegral (10^s) in fromIntegral (round (n * factor)) / factor

-- | > (XX)
--
-- > Explode stuff
builtinExplode :: BlsqState
builtinExplode = do
 st <- get
 putResult $
  case st of
   (BlsqStr a : xs) -> (BlsqBlock $ map (BlsqChar) a) : xs
   (BlsqInt a : xs) -> (BlsqBlock $ map (\c -> BlsqInt . read $ [c]) (show (abs a))) : xs
   (BlsqDouble a : xs) -> (BlsqBlock [BlsqInt . floor $ a, BlsqInt . ceiling $ a]) : xs
   (BlsqChar a : xs) -> (BlsqStr [a]) : xs
   _ -> BlsqError "Burlesque: (XX) Invalid arguments!" : st
