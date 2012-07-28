module Burlesque.Eval
  (eval, run, runStack, builtins)
 where

import Burlesque.Types
import Burlesque.Parser
import Burlesque.Helpers
import Burlesque.Display

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char
import Data.Bits
import Text.Regex

import Debug.Trace

-- | > Evaluate a Burlesque program
eval :: BlsqProg -> BlsqState
eval (x:xs) = evalI x >> eval xs
eval [] = return ()

evalI v@(BlsqSpecial ",") = do
 st <- get
 if length st == 1 then
   do put []
 else return ()
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
  ("WL", builtinWithLinesPretty),
  ("wL", builtinWithLinesParsePretty),
  ("ri", builtinReadInt),
  ("rd", builtinReadDouble),
  ("ra", builtinReadArray),
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
  ("~-", builtinInitTail),
  ("-]", builtinHead),
  ("[+", builtinAppend),
  ("+]", builtinPrepend),
  ("\\[", builtinConcat),
  ("[[", builtinIntersperse),
  ("m[", builtinMap),
  ("r[", builtinReduce),
  ("\\/", builtinSwap),
  ("^^", builtinDup),
  ("vv", builtinPop),
  ("XX", builtinExplode),
  ("~[", builtinContains),
  ("~~", builtinInfixOf),
  ("~!", builtinPrefixOf),
  ("!~", builtinSuffixOf),
  ("r~", builtinReplace),
  ("R~", builtinReplaceRegex),
  ("^p", builtinPushMany),
  ("p^", builtinPushManyReverse),
  ("=[", builtinGroup),
  ("sh", builtinPretty),
  ("FF", builtinFormat),
  ("ff", builtinFromFormat),
  ("Ff", builtinFormatFromFormat),
  ("SH", builtinPrettyFormatFromFormat),
  ("Sh", builtinPrettyFromFormat),
  ("~=", builtinMatches),
  ("=~", builtinMatchesList),
  ("||", builtinOr),
  ("&&", builtinAnd),
  ("$$", builtinXor),
  ("L[", builtinLength),
  ("ab", builtinAbs),
  ("sn", builtinSignum),
  ("S[", builtinStripLeft),
  ("[S", builtinStripRight),
  ("P[", builtinPadLeft),
  ("[P", builtinPadRight),
  (";;", builtinSplit),
  ("UN", builtinUnion),
  ("IN", builtinIntersection),
  ("NB", builtinNub),
  ("\\\\", builtinDiffLs),
  ("r@", builtinRange),
  ("bx", builtinBox),
  ("><", builtinSort),
  ("/v", builtinSwapPop),
  ("v/", builtinPopSwap),
  ("^/", builtinDupSwap),
  ("/^", builtinSwapDup),
  ("r&", builtinAndLs),
  ("r|", builtinOrLs)
 ]

lookupBuiltin b = fromMaybe (return ()) $ lookup b builtins

putResult = put

-- | > .+
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
builtinDiv :: BlsqState
builtinDiv = do
 st <- get
 putResult $
  case st of
    (BlsqInt b : BlsqInt a : xs) -> BlsqInt (a `div` b) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a / b)) : xs
    _ -> (BlsqError "Burlesque: (./) Invalid arguments!") : st

-- | > +.
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
builtinReverse :: BlsqState
builtinReverse = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqStr $ reverse a) : xs
   (BlsqInt a) : xs -> (BlsqInt . read . reverse . show $ a) : xs
   (BlsqBlock a) : xs -> (BlsqBlock $ reverse a) : xs
   (BlsqChar a : xs) -> (BlsqChar (if isUpper a then toLower a else toUpper a)) : xs
   _ -> (BlsqError "Burlesque: (<-) Invalid arguments!") : st

-- | > ln
builtinLines :: BlsqState
builtinLines = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqBlock . map BlsqStr . lines $ a) : xs
   (BlsqInt a) : xs -> (BlsqInt . genericLength . show $ a) : xs
   _ -> (BlsqError "Burlesque: (ln) Invalid arguments!") : st

-- | > un
builtinUnlines :: BlsqState
builtinUnlines = do
 modify (BlsqStr "\n" :)
 builtinSwap
 builtinIntersperse
 builtinConcat

-- | > [[
builtinIntersperse :: BlsqState
builtinIntersperse = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : a : xs) -> (BlsqBlock $ intersperse a b) : xs
   _ -> (BlsqError "Burlesque: ([[) Invalid arguments!") : st

-- | > ri
builtinReadInt :: BlsqState
builtinReadInt = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqInt . read $ a) : xs
   (BlsqInt a) : xs -> (BlsqInt a) : xs
   (BlsqChar a) : xs -> BlsqInt (if isAlphaNum a then 1 else 0) : xs
   _ -> (BlsqError "Burlesque: (ri) Invalid arguments!") : st

-- | > rd
builtinReadDouble :: BlsqState
builtinReadDouble = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqDouble . read $ a) : xs
   (BlsqDouble a) : xs -> (BlsqDouble a) : xs
   (BlsqChar a) : xs -> BlsqInt (if isAlpha a then 1 else 0) : xs
   _ -> (BlsqError "Burlesque: (rd) Invalid arguments!") : st

-- | > ra
builtinReadArray :: BlsqState
builtinReadArray = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (runParserWithString' parseData a) : xs
   (BlsqChar a) : xs -> BlsqInt (if isSpace a then 1 else 0) :xs
   _ -> (BlsqError "Burlesque: (ra) Invalid arguments!") : st

-- | > ps
builtinParse :: BlsqState
builtinParse = do
 st <- get
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqBlock (runParserWithString parseBlsq a)) : xs
   _ -> (BlsqError "Burlesque: (ps) Invalid arguments!") : st

-- | > ++
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
builtinLast :: BlsqState
builtinLast = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> last a : xs
   (BlsqStr a) : xs -> BlsqChar (last a) : xs
   (BlsqInt a) : xs -> BlsqInt (read . ls . last . show . abs $ a) : xs
   _ -> (BlsqError "Burlesque: ([~) Invalid arguments!") : st
 where ls a = [a]

-- | > ~]
builtinInit :: BlsqState
builtinInit = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> (BlsqBlock (init a)) : xs
   (BlsqStr a) : xs -> BlsqStr (init a) : xs
   (BlsqInt a) : xs -> BlsqInt (read . init . show . abs $ a) : xs
   _ -> (BlsqError "Burlesque: (~]) Invalid arguments!") : st

-- | > [-
builtinTail :: BlsqState
builtinTail = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> (BlsqBlock (tail a)) : xs
   (BlsqStr a) : xs -> BlsqStr (tail a) : xs
   (BlsqInt a) : xs -> BlsqInt (read . tail . show . abs $ a) : xs
   _ -> (BlsqError "Burlesque: ([-) Invalid arguments!") : st

-- | ~-
builtinInitTail :: BlsqState
builtinInitTail = do
 builtinInit
 builtinTail

-- | > -]
builtinHead :: BlsqState
builtinHead = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a) : xs -> head a : xs
   (BlsqStr a) : xs -> BlsqChar (head a) : xs
   (BlsqInt a) : xs -> BlsqInt (read . ls . head . show . abs $ a) : xs
   _ -> (BlsqError "Burlesque: (-]) Invalid arguments!") : st
 where ls a = [a]

-- | > [+
builtinAppend :: BlsqState
builtinAppend = do
 st <- get
 putResult $
  case st of
   (b : BlsqBlock a : xs) -> BlsqBlock (a ++ [b]) : xs
   (BlsqChar b : BlsqStr a : xs) -> BlsqStr (a ++ [b]) : xs
   _ -> (BlsqError "Burlesque: ([+) Invalid arguments!") : st

-- | > +]
builtinPrepend :: BlsqState
builtinPrepend = do
 st <- get
 putResult $
  case st of
   (b : BlsqBlock a : xs) -> BlsqBlock (b : a) : xs
   (BlsqChar b : BlsqStr a : xs) -> BlsqStr (b : a) : xs
   _ -> (BlsqError "Burlesque: (+]) Invalid arguments!") : st

-- | > \[
builtinConcat :: BlsqState
builtinConcat = do
 st <- get
 case st of
  (BlsqBlock a) : xs -> do
     modify ((BlsqBlock $ [BlsqIdent ".+"]) :)
     builtinReduce
  _ -> putResult $ (BlsqError "Burlesque: (\\[) Invalid arguments!") : st

-- | > m[
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

-- | > r[
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
builtinWithLines :: BlsqState
builtinWithLines = do
 builtinSwap
 builtinLines
 builtinSwap
 builtinMap
 builtinUnlines

-- | > WL
builtinWithLinesPretty :: BlsqState
builtinWithLinesPretty = do
 builtinWithLines
 builtinPretty

-- | > wL
builtinWithLinesParsePretty :: BlsqState
builtinWithLinesParsePretty = do
 modify (BlsqIdent "ps" :)
 builtinPrepend
 builtinWithLinesPretty
 

-- | > \/
builtinSwap :: BlsqState
builtinSwap = do
 st <- get
 putResult $
  case st of
   (a : b : xs) -> b : a : xs
   _ -> BlsqError "Burlesque: (\\/) Stack size error!" : st

-- | > ^^
builtinDup :: BlsqState
builtinDup = do
 st <- get
 putResult $
  case st of
   (a : xs) -> (a : a : xs)
   _ -> BlsqError "Burlesque: (^^) Stack size error!" : st

-- | > vv
builtinPop :: BlsqState
builtinPop = do
 st <- get
 putResult $
  case st of
   (a : xs) -> xs
   _ -> BlsqError "Burlesque: (vv) Stack size error!" : st

-- | > if
builtinIff :: BlsqState
builtinIff = do
 st <- get
 putResult $
  case st of
   (BlsqInt 0 : BlsqBlock b : xs) -> xs
   (BlsqInt _ : BlsqBlock b : xs) -> runStack b xs
   _ -> BlsqError "Burlesque: (if) Invalid arguments!" : st

-- | > ie
builtinIfElse:: BlsqState
builtinIfElse = do
 st <- get
 putResult $
  case st of
   (BlsqInt 0 : BlsqBlock b : BlsqBlock a : xs) -> runStack b xs
   (BlsqInt _ : BlsqBlock b : BlsqBlock a : xs) -> runStack a xs
   _ -> BlsqError "Burlesque: (ie Invalid arguments!" : st

-- | > e!
builtinEval :: BlsqState
builtinEval = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : xs) -> runStack b xs
   _ -> BlsqError "Burlesque: (e!) Invalid arguments!" : st

-- | > c!
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
builtinGreater :: BlsqState
builtinGreater = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a > b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (.>) Invalid arguments!" : st

-- | > (.<)
builtinSmaller :: BlsqState
builtinSmaller = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a < b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (.<) Invalid arguments!" : st

-- | > (>.)
builtinMax :: BlsqState
builtinMax = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (max a b) : xs
   _ -> BlsqError "Burlesque: (>.) Stack size error!" : st

-- | > (<.)
builtinMin :: BlsqState
builtinMin = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (min a b) : xs
   _ -> BlsqError "Burlesque: (<.) Stack size error!" : st

-- | > (>])
builtinMaximum :: BlsqState
builtinMaximum = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : xs) -> (maximum a) : xs
   _ -> BlsqError "Burlesque: (>]) Invalid arguments!" : st

-- | > (<])
builtinMinimum :: BlsqState
builtinMinimum = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : xs) -> (minimum a) : xs
   _ -> BlsqError "Burlesque: (<]) Invalid arguments!" : st


-- | > (==)
builtinEqual :: BlsqState
builtinEqual = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a == b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (==) Invalid arguments!" : st

-- | > (r_)
builtinRound :: BlsqState
builtinRound = do
 st <- get
 putResult $
  case st of
   (BlsqInt b : BlsqDouble a : xs) -> (BlsqDouble $ round' a b) : xs
   _ -> BlsqError "Burlesque: (r_) Invalid arguments!" : st
 where round' n s = let factor = fromIntegral (10^s) in fromIntegral (round (n * factor)) / factor

-- | > (XX)
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

-- | > sh
builtinPretty :: BlsqState
builtinPretty = do
 st <- get
 putResult $
  case st of
   (a : xs) -> (BlsqPretty a BlsqFormatNormal) : xs
   _ -> BlsqError "Burlesque: (sh) Invalid arguments!" : st

-- | > ~[
builtinContains :: BlsqState
builtinContains = do
 st <- get
 putResult $
  case st of
   (a : BlsqBlock ls : xs) -> BlsqInt (if a `elem` ls then 1 else 0) : xs
   (BlsqChar a : BlsqStr ls : xs) -> BlsqInt (if a `elem` ls then 1 else 0) : xs
   (BlsqInt a : BlsqInt ls : xs) -> BlsqInt (if (show $ abs a) `isInfixOf` (show $ abs ls) then 1 else 0) : xs
   (BlsqStr a : BlsqStr ls : xs) -> BlsqInt (if a `isInfixOf` ls then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (~[) Invalid arguments!" : st

-- | > ~~
builtinInfixOf :: BlsqState
builtinInfixOf = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> BlsqInt (if a `isInfixOf` b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (~~) Invalid arguments!" : st

-- | > ~!
builtinPrefixOf :: BlsqState
builtinPrefixOf = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> BlsqInt (if a `isPrefixOf` b then 1 else 0) : xs
   (BlsqStr a : BlsqStr b : xs) -> BlsqInt (if a `isPrefixOf` b then 1 else 0) : xs
   (BlsqInt a : BlsqInt b : xs) -> BlsqInt (if (show . abs $ a) `isPrefixOf` (show . abs $ b) then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (~!) Invalid arguments!" : st

-- | > !~
builtinSuffixOf :: BlsqState
builtinSuffixOf = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> BlsqInt (if a `isSuffixOf` b then 1 else 0) : xs
   (BlsqStr a : BlsqStr b : xs) -> BlsqInt (if a `isSuffixOf` b then 1 else 0) : xs
   (BlsqInt a : BlsqInt b : xs) -> BlsqInt (if (show . abs $ a) `isSuffixOf` (show . abs $ b) then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (!~) Invalid arguments!" : st

-- | > r~
builtinReplace :: BlsqState
builtinReplace = do
 st <- get
 putResult $
  case st of
   (n : o : BlsqBlock ls : xs) -> (BlsqBlock $ replace [o] [n] ls) : xs
   (BlsqChar n : BlsqChar o : BlsqStr ls : xs) -> (BlsqStr $ replace [o] [n] ls) : xs
   (BlsqStr n : BlsqStr o : BlsqStr ls : xs) -> (BlsqStr $ replace o n ls) : xs
   (BlsqInt n : BlsqInt o : BlsqInt ls : xs) -> 
       (BlsqInt . read $ replace (show . abs $ o) (show . abs $ n) (show . abs $ ls)) : xs
   _ -> BlsqError "Burlesque: (r~) Invalid arguments!" : st

-- | > ^p
builtinPushMany :: BlsqState
builtinPushMany = do
 st <- get
 case st of
  (BlsqBlock ls : xs) -> do
     builtinPop
     mapM (\v -> modify (v:)) ls
 return ()

-- | > p^
builtinPushManyReverse :: BlsqState
builtinPushManyReverse = do
 st <- get
 case st of
  (BlsqBlock ls : xs) -> do 
     builtinPop
     mapM (\v -> modify (v:)) (reverse ls)
 return ()

-- | > =[
builtinGroup :: BlsqState
builtinGroup = do
 st <- get
 putResult $
  case st of
   (BlsqBlock ls : xs) -> (BlsqBlock $ map (BlsqBlock) (group ls)) : xs
   _ -> BlsqError "Burlesque: (=[) Invalid arguments!" : st

-- | > FF
builtinFormat :: BlsqState
builtinFormat = do
 st <- get
 putResult $
  case st of
   (BlsqInt 0 : BlsqPretty x _ : xs) -> (BlsqPretty x BlsqFormatNormal) : xs
   (BlsqInt 1 : BlsqPretty x _ : xs) -> (BlsqPretty x BlsqFormatNoSpaces) : xs
   (BlsqInt 2 : BlsqPretty x _ : xs) -> (BlsqPretty x BlsqFormatWithSpaces) : xs
   _ -> BlsqError "Burlesque: (FF) Invalid arguments!" : st

-- | > ff
builtinFromFormat :: BlsqState
builtinFromFormat = do
 st <- get
 putResult $
  case st of
   (BlsqPretty x f : xs) -> (BlsqStr $ (toDisplay $ BlsqPretty x f)) : xs
   _ -> BlsqError "Burlesque: (ff) Invalid arguments!" : st

-- | > Ff
builtinFormatFromFormat :: BlsqState
builtinFormatFromFormat = do
 builtinFormat
 builtinFromFormat

-- | > SH
builtinPrettyFormatFromFormat :: BlsqState
builtinPrettyFormatFromFormat = do
 builtinSwap
 builtinPretty
 builtinSwap
 builtinFormatFromFormat

-- | > Sh
builtinPrettyFromFormat :: BlsqState
builtinPrettyFromFormat = do
 builtinPretty
 builtinFromFormat

-- | > ~=
builtinMatches :: BlsqState
builtinMatches = do
 st <- get
 putResult $
  case st of
   (BlsqStr regex : BlsqStr str : xs) -> (case matchRegex (mkRegex regex) str of
                                            Just q -> BlsqInt 1
                                            _ -> BlsqInt 0) : xs
   _ -> BlsqError "Burlesque: (~=) Invalid arguments!" : st

-- | > =~
builtinMatchesList :: BlsqState
builtinMatchesList = do
 st <- get
 putResult $
  case st of
   (BlsqStr regex : BlsqStr str : xs) -> (case matchRegex (mkRegex regex) str of
                                            Just q -> BlsqBlock $ map BlsqStr q
                                            _ -> BlsqBlock []) : xs
   _ -> BlsqError "Burlesque: (~=) Invalid arguments!" : st

-- | > R~
builtinReplaceRegex :: BlsqState
builtinReplaceRegex = do
 st <- get
 putResult $
  case st of
   (BlsqStr regex : BlsqStr repl : BlsqStr str : xs) -> 
           BlsqStr (subRegex (mkRegex regex) str repl) : xs
   _ -> BlsqError "Burlesque: (~=) Invalid arguments!" : st

-- | ||
builtinOr :: BlsqState
builtinOr = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : BlsqInt b : xs) -> (BlsqInt $ a .|. b) : xs
   _ -> BlsqError "Burlesque: (||) Invalid arguments!": st

-- | &&
builtinAnd :: BlsqState
builtinAnd = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : BlsqInt b : xs) -> (BlsqInt $ a .&. b) : xs
   _ -> BlsqError "Burlesque: (&&) Invalid arguments!": st

-- | $$
builtinXor :: BlsqState
builtinXor = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : BlsqInt b : xs) -> (BlsqInt $ a `xor` b) : xs
   _ -> BlsqError "Burlesque: ($$) Invalid arguments!": st

-- | L[
builtinLength :: BlsqState
builtinLength = do
 st <- get
 putResult $
  case st of
   (BlsqStr a : xs) -> (BlsqInt $ genericLength a) : xs
   (BlsqBlock a : xs) -> (BlsqInt $ genericLength a) : xs
   (BlsqInt a : xs) -> (BlsqChar $  chr (fromInteger a)) : xs
   (BlsqChar a : xs) -> (BlsqChar (if isUpper a then 'A' else 'a')) : xs
   _ -> BlsqError "Burlesque: (L[) Invalid arguments!" : st

-- | ab
builtinAbs :: BlsqState
builtinAbs = do
 st <- get
 putResult $ 
  case st of
   (BlsqInt a : xs) -> (BlsqInt $ abs a) : xs
   (BlsqDouble a : xs) -> (BlsqDouble $ abs a) : xs
   _ -> BlsqError "Burlesque: (ab) Invalid arguments!" : st

-- | sn
builtinSignum :: BlsqState
builtinSignum = do
 st <- get
 putResult $ 
  case st of
   (BlsqInt a : xs) -> (BlsqInt $ signum a) : xs
   (BlsqDouble a : xs) -> (BlsqDouble $ signum a) : xs
   _ -> BlsqError "Burlesque: (sn) Invalid arguments!" : st

-- | S[
builtinStripLeft :: BlsqState
builtinStripLeft = do
 st <- get
 putResult $
  case st of
   (a : BlsqBlock b : xs) -> (BlsqBlock $ dropWhile (==a) b) : xs
   (BlsqChar a : BlsqStr b : xs) -> (BlsqStr $ dropWhile (==a) b) : xs
   _ -> BlsqError "Burlesque: (S[) Invalid arguments!" : st

-- | S[
builtinStripRight :: BlsqState
builtinStripRight = do
 st <- get
 putResult $
  case st of
   (a : BlsqBlock b : xs) -> (BlsqBlock .reverse $ dropWhile (==a) (reverse b)) : xs
   (BlsqChar a : BlsqStr b : xs) -> (BlsqStr . reverse $ dropWhile (==a) (reverse b)) : xs
   _ -> BlsqError "Burlesque: ([S) Invalid arguments!" : st

-- | P[
builtinPadLeft :: BlsqState
builtinPadLeft = do
 st <- get
 putResult $
  case st of
   (a : BlsqInt b : BlsqBlock ls : xs) -> BlsqBlock  
                                         (if genericLength ls >= b then
                                            genericTake b ls
                                          else (genericReplicate (b-(genericLength ls)) a) ++ ls) :xs
   (BlsqChar a : BlsqInt b : BlsqStr ls : xs) -> BlsqStr 
                                         (if genericLength ls >= b then
                                            genericTake b ls
                                          else (genericReplicate (b-(genericLength ls)) a) ++ ls) :xs
   _ -> BlsqError "Burlesque: (P[) Invalid arguments!" : st

-- | [P
builtinPadRight :: BlsqState
builtinPadRight = do
 st <- get
 putResult $
  case st of
   (a : BlsqInt b : BlsqBlock ls : xs) -> BlsqBlock 
                                         (if genericLength ls >= b then
                                            genericTake b ls
                                          else ls ++ (genericReplicate (b-(genericLength ls)) a)) :xs
   (BlsqChar a : BlsqInt b : BlsqStr ls : xs) -> BlsqStr 
                                         (if genericLength ls >= b then
                                            genericTake b ls
                                          else ls ++ (genericReplicate (b-(genericLength ls)) a)) :xs
   _ -> BlsqError "Burlesque: ([P) Invalid arguments!" : st

-- | ;;
builtinSplit :: BlsqState
builtinSplit = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> BlsqBlock (map BlsqBlock (splitOn a b)) : xs
   (BlsqStr a : BlsqStr b : xs) -> BlsqBlock (map BlsqStr (splitOn a b)) : xs
   (BlsqInt a : BlsqInt b : xs) -> BlsqBlock (map (BlsqInt . read) (splitOn (show (abs a)) (show (abs b)))) : xs
   _ -> BlsqError "Burlesque: (;;) Invalid arguments!" : st

-- | UN
builtinUnion :: BlsqState
builtinUnion = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (union a b) : xs
   (BlsqStr b : BlsqStr a : xs) -> BlsqStr (union a b) : xs
   (BlsqInt b : BlsqInt a : xs) -> BlsqInt (read $ union (show (abs a)) (show (abs b))) : xs
   _ -> BlsqError "Burlesque: (UN) Invalid arguments!" : st

-- | IN
builtinIntersection :: BlsqState
builtinIntersection = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (intersect a b) : xs
   (BlsqStr b : BlsqStr a : xs) -> BlsqStr (intersect a b) : xs
   (BlsqInt b : BlsqInt a : xs) -> BlsqInt (read $ intersect (show (abs a)) (show (abs b))) : xs
   _ -> BlsqError "Burlesque: (IN) Invalid arguments!" : st

-- | NB
builtinNub :: BlsqState
builtinNub = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : xs) -> BlsqBlock (nub a) : xs
   (BlsqStr a : xs) -> BlsqStr (nub a) : xs
   (BlsqInt a : xs) -> BlsqInt (read $ nub (show (abs a))) : xs
   _ -> BlsqError "Burlesque: (NB) Invalid arguments!" : st

-- | \\
builtinDiffLs :: BlsqState
builtinDiffLs = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (a \\ b) : xs
   (BlsqStr b : BlsqStr a : xs) -> BlsqStr (a \\ b) : xs
   (BlsqInt b : BlsqInt a : xs) -> BlsqInt (read $ (show (abs a)) \\ (show (abs b))) : xs
   _ -> BlsqError "Burlesque: (\\\\) Invalid arguments!" : st

-- | r@
builtinRange :: BlsqState
builtinRange = do
 st <- get
 putResult $
  case st of
   (BlsqInt b : BlsqInt a : xs) -> BlsqBlock (map BlsqInt [a..b]) : xs
   (BlsqChar b : BlsqChar a : xs) -> BlsqBlock (map BlsqChar [a..b]) : xs
   (BlsqDouble a : xs) -> BlsqDouble (sqrt a) : xs
   (BlsqStr a : xs) -> BlsqBlock (map BlsqStr (permutations a)) : xs
   (BlsqBlock a : xs) -> BlsqBlock (map BlsqBlock (permutations a)) : xs
   _ -> BlsqError "Burlesque: (r@) Invalid arguments!" : st

-- | > bx
builtinBox :: BlsqState
builtinBox = do
 st <- get
 putResult $
  case st of
   (a : xs) -> (BlsqBlock [a]) : xs
   _ -> BlsqError "Burlesque: (bx) Invalid arguments!" : st

-- | > ><
builtinSort :: BlsqState
builtinSort = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : xs) -> BlsqBlock (sort a) : xs
   (BlsqStr a : xs) -> BlsqStr (sort a) : xs
   (BlsqInt a : xs) -> BlsqStr (read.sort.show.abs $ a) : xs
   (BlsqChar a : xs) -> BlsqInt (if isDigit a then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (><) Invalid arguments!" : st

-- | > /v
builtinSwapPop :: BlsqState
builtinSwapPop = do
 builtinSwap
 builtinPop

-- | > v/
builtinPopSwap :: BlsqState
builtinPopSwap = do
 builtinPop
 builtinSwap

-- | > ^/
builtinDupSwap :: BlsqState
builtinDupSwap = do
 builtinDup
 builtinSwap

-- | > /^
builtinSwapDup :: BlsqState
builtinSwapDup = do
 builtinSwap
 builtinDup

-- | > r&
builtinAndLs :: BlsqState
builtinAndLs = do
 modify(BlsqBlock [(BlsqIdent "&&")] :)
 builtinReduce

-- | > r|
builtinOrLs :: BlsqState
builtinOrLs = do
 modify(BlsqBlock [(BlsqIdent "||")] :)
 builtinReduce
