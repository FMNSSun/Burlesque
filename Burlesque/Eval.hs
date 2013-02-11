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
import Data.Ord
import Text.Regex

import Control.Monad

import System.Random

import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Distribution.Binomial
import Statistics.Distribution.Poisson

import Debug.Trace

-- | > Evaluate a Burlesque program
eval :: BlsqProg -> BlsqState
eval (BlsqHackMode x : xs) = do
 let m = map (\c -> BlsqIdent . fst $ builtins !! (ord c)) x
 eval (m ++ xs)
 
eval (x:xs) = evalI x >> eval xs
eval [] = return ()

evalI (BlsqQuoted q) = modify (q:)
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

-- Very Important!
-- The order of this list is relevant!
-- If you change the order of this list you'll break existing
-- programs making use of the hack mode. DO NOT CHANGE THE ORDER
-- OF THIS LIST! JUST DO NOT!
-- HackMode will not make it into an official version. Screw the order!
builtins = [
  (".+", builtinAdd),
  ("_+", builtinAddX),
  (".-", builtinSub),
  ("./", builtinDiv),
  (".*", builtinMul),
  (".%", builtinMod),
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
  ("R_", builtinRound2),
  ("==", builtinEqual),
  ("!=", builtinNotEqual),
  ("<-", builtinReverse),
  ("ln", builtinLines),
  ("un", builtinUnlines),
  ("uN", builtinUnlinesPretty),
  ("wl", builtinWithLines),
  ("WL", builtinWithLinesPretty),
  ("wL", builtinWithLinesParsePretty),
  ("ri", builtinReadInt),
  ("rd", builtinReadDouble),
  ("ra", builtinReadArray),
  ("ps", builtinParse),
  ("up", builtinUnparse),
  ("if", builtinIff),
  ("ie", builtinIfElse),
  ("e!", builtinEval),
  ("E!", builtinEvalMany),
  ("c!", builtinContinuation),
  ("w!", builtinWhile),
  ("++", builtinSum),
  ("pd", builtinProduct),
  ("PD", builtinProductMany),
  ("av", builtinAverage),
  ("AV", builtinAverage2),
  ("[~", builtinLast),
  ("~]", builtinInit),
  ("[-", builtinTail),
  ("~-", builtinInitTail),
  ("-~", builtinHeadTail),
  ("-]", builtinHead),
  ("[+", builtinAppend),
  ("+]", builtinPrepend),
  ("\\[", builtinConcat),
  ("\\m", builtinConcatMap),
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
  ("sH", builtinPrettyPretty),
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
  ("R@", builtinRangeInf),
  ("bx", builtinBox),
  ("><", builtinSort),
  ("<>", builtinSortReverse),
  ("/v", builtinSwapPop),
  ("v/", builtinPopSwap),
  ("^/", builtinDupSwap),
  ("/^", builtinSwapDup),
  ("r&", builtinAndLs),
  ("r|", builtinOrLs),
  ("ZZ", builtinToUpper),
  ("zz", builtinToLower),
  ("M[", builtinMapPretty),
  ("M]", builtinMapToPretty),
  ("m]", builtinMapToPrettyFromFormat),
  ("[m", builtinMapDup),
  ("]m", builtinMapString),
  ("[M", builtinMapParse),
  ("wd", builtinWords),
  ("wD", builtinWords3),
  ("f[", builtinFilter),
  ("z[", builtinZip),
  ("Z[", builtinZipWith),
  ("Z]", builtinZipWithPush),
  ("!!", builtinBlockAccess),
  ("fi", builtinFindIndex),
  ("Fi", builtinFindIndexEq),
  ("fI", builtinFindIndices),
  ("fe", builtinFindElement),
  ("CB", builtinCombinations),
  ("cb", builtinCombinationsUpTo),
  ("cy", builtinCycle),
  ("is", builtinIsError),
  ("fc", builtinFactors),
  ("fC", builtinPrimeFactors),
  ("co", builtinChunksOf),
  ("CO", builtinChunky),
  ("t[", builtinTrimLeft),
  ("t]", builtinTrimRight),
  ("tt", builtinTrimLeftRight),
  ("n!", builtinNot),
  ("lg", builtinLog),
  ("LG", builtinLog2),
  ("Ts", builtinSin),
  ("TS", builtinAsin),
  ("Tc", builtinCos),
  ("TC", builtinAcos),
  ("Tt", builtinTan), 
  ("TT", builtinAtan),
  ("WD", builtinWords2),
  ("ia", builtinInsertAt),
  ("RA", builtinRemoveAt),
  ("sa", builtinSetAt),
  ("sb", builtinSortBy),
  ("cm", builtinCompare),
  ("CM", builtinCompare2),
  ("Cm", builtinCompare3),
  ("B!", builtinConvertBase),
  ("g_", builtinGcd),
  ("l_", builtinLcm),
  ("tw", builtinTakeWhile),
  ("dw", builtinDropWhile),
  ("tp", builtinTranspose),
  ("FM", builtinFilterMap),
  ("r\\", builtinRangeConcat),
  ("SP", builtinSpecialInput),
  ("hd", builtinHide),
  ("HD", builtinHide2),
  ("ld", builtinLoad),
  ("LD", builtinLoad2),
  ("st", builtinStore),
  ("#a", builtinALoad),
  ("#b", builtinBLoad),
  ("#c", builtinCLoad),
  ("`a", builtinAStore),
  ("`b", builtinBStore),
  ("`c", builtinCStore),
  ("!a", builtinALoad2),
  ("!b", builtinBLoad2),
  ("!c", builtinCLoad2),
  ("sc", builtinSortByComparing),
  ("?+", builtinCoerceAdd),
  ("?-", builtinCoerceSub),
  ("?/", builtinCoerceDiv),
  ("?*", builtinCoerceMul),
  ("im", builtinImplode),
  ("ms", builtinMapSum),
  ("mp", builtinMapProduct),
  ("sg", builtinSortGroup),
  ("gs", builtinGroupSort),
  ("cp", builtinCrossProduct),
  ("bc", builtinBoxCycle),
  ("rt", builtinRotate),
  ("RT", builtinRotate2),
  ("d!", builtinDimArrayAccess),
  ("D!", builtinDimArraySet),
  ("Wl", builtinWithLinesString),
  ("si", builtinSelectIndices),
  ("ro", builtinRangeFromOne),
  ("rz", builtinRangeFromZero),
  ("nu", builtinNull),
  ("fl", builtinFilterLength),
  ("to", builtinTypeOf),
  ("sr", builtinSplitRegex),
  ("rn", builtinRandomInts),
  ("RN", builtinRandomDoubles),
  (">m", builtinMaximumBy),
  ("<m", builtinMinimumBy),
  ("nc", builtinNormalDCumulative),
  ("nd", builtinNormalDDensity),
  ("Bc", builtinBinomialDCumulative),
  ("Bp", builtinBinomialDProbability),
  ("pc", builtinPoissonDCumulative),
  ("pp", builtinPoissonDProbability),
  ("gr", builtinGrep),
  ("pm", builtinPlusMinus),
  
  ("??", builtinVersion)
 ]

lookupBuiltin b = fromMaybe (modify (BlsqError ("Unknown command: (" ++ b ++ ")!") :)) $ lookup b builtins

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
    ((BlsqBlock b):(BlsqInt a):xs) -> (BlsqBlock $ genericTake a b) : xs
    _ -> (BlsqError $ "Burlesque: (.+) Invalid arguments!") : st

-- | > _+
builtinAddX :: BlsqState
builtinAddX = do
 st <- get
 putResult $
  case st of
    ((BlsqInt b):(BlsqInt a):xs) -> (BlsqBlock [BlsqInt a, BlsqInt b]) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqBlock [BlsqDouble a, BlsqDouble b]) : xs
    ((BlsqStr b):(BlsqStr a):xs) -> (BlsqStr (a ++ b)) : xs
    (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (a ++ b) : xs
    (BlsqChar b : BlsqChar a : xs) -> (BlsqStr $ a:b:"") : xs
    (BlsqChar b : BlsqStr a : xs) -> (BlsqStr $ a++[b]) : xs
    (BlsqStr b : BlsqChar a : xs) -> (BlsqStr $ b++[a]) : xs
    (BlsqInt b : BlsqStr a : xs) -> (BlsqStr $ a++ show b) : xs
    (BlsqStr b : BlsqInt a : xs) -> (BlsqStr $ b++ show a) : xs
    (a : BlsqBlock b : xs) -> (BlsqBlock $ b++[a]) : xs
    _ -> (BlsqError $ "Burlesque: (_+) Invalid arguments!") : st

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
    ((BlsqBlock b):(BlsqInt a):xs) -> (BlsqBlock $ genericDrop a b) : xs
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
    (BlsqStr a : BlsqStr b : xs) -> BlsqStr (reverse $ a++b) : xs
    _ -> (BlsqError "Burlesque: (.*) Invalid arguments!") : st

-- | > ./
builtinDiv :: BlsqState
builtinDiv = do
 st <- get
 putResult $
  case st of
    (BlsqInt b : BlsqInt a : xs) -> BlsqInt (a `div` b) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a / b)) : xs
    (BlsqStr a : BlsqStr b : xs) -> case a `isPrefixOf` b of
                                 True -> BlsqStr (drop (length a) b) : xs
                                 False -> BlsqStr b : xs
    _ -> (BlsqError "Burlesque: (./) Invalid arguments!") : st

-- | .%
builtinMod :: BlsqState
builtinMod = do
  st <- get
  putResult $
   case st of
    (BlsqInt b : BlsqInt a : xs) -> BlsqInt (a `mod`b) : xs
    _ -> (BlsqError "Burlesque: (.%) Invalid arguments!") : st

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

-- | > uN
builtinUnlinesPretty :: BlsqState
builtinUnlinesPretty = do
 builtinUnlines
 builtinPretty

-- | > [[
builtinIntersperse :: BlsqState
builtinIntersperse = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : a : xs) -> (BlsqBlock $ intersperse a b) : xs
   (BlsqStr b : BlsqChar a : xs) -> (BlsqStr $ intersperse a b) : xs
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

-- | > up
builtinUnparse :: BlsqState
builtinUnparse = do
 st <- get
 putResult $
  case st of 
   (a : xs) -> BlsqStr (toDisplay a) : xs
   _ -> (BlsqError "Burlesque: (up) Invalid arguments!") : st

-- | > ++
builtinSum :: BlsqState
builtinSum = do
 st <- get
 case st of
   (BlsqBlock a) : xs -> do modify (BlsqBlock [BlsqIdent ".+"] : )
                            builtinReduce
   (BlsqInt b : BlsqInt a : xs) -> putResult $ (BlsqInt . read $ (show (abs a)) ++ (show (abs b))) : xs
   _ -> putResult $ (BlsqError "Burlesque: (++) Invalid arguments!") : st

builtinProduct :: BlsqState
builtinProduct = do
 st <- get
 case st of
   (BlsqBlock a) : xs -> do modify (BlsqBlock [BlsqIdent ".*"] : )
                            builtinReduce
   (BlsqInt _ : xs) -> do builtinPrettyFromFormat
                          builtinReadDouble
   (BlsqDouble a : xs) -> do putResult $ (BlsqInt . ceiling $ a) : xs
   _ -> putResult $ (BlsqError "Burlesque: (pd) Invalid arguments!"): st

builtinProductMany :: BlsqState
builtinProductMany = do modify(BlsqBlock [BlsqIdent "pd"] :)
                        builtinMap

builtinAverage :: BlsqState
builtinAverage = do
 st <- get
 case st of
  (BlsqBlock a) : xs -> do builtinDup
                           builtinSum
                           builtinSwap
                           builtinLength
                           builtinProduct
                           builtinDiv
  (BlsqDouble a : xs) -> do putResult $ (BlsqInt . floor $ a) : xs
  _ -> putResult $ (BlsqError "Burlesque: (av) Invalid arguments!") : st

builtinAverage2 :: BlsqState
builtinAverage2 = do
 builtinProductMany
 builtinAverage

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
   (BlsqChar a) : xs -> BlsqStr [a] : xs
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
   (BlsqInt b : BlsqInt a : xs) -> (BlsqInt . read $ (show . abs $ a) ++ (show . abs $ b)) : xs
   _ -> (BlsqError "Burlesque: ([+) Invalid arguments!") : st

-- | > +]
builtinPrepend :: BlsqState
builtinPrepend = do
 st <- get
 putResult $
  case st of
   (b : BlsqBlock a : xs) -> BlsqBlock (b : a) : xs
   (BlsqChar b : BlsqStr a : xs) -> BlsqStr (b : a) : xs
   (BlsqInt b : BlsqInt a : xs) -> (BlsqInt . read $ (show . abs $ b) ++ (show . abs $ a)) : xs
   _ -> (BlsqError "Burlesque: (+]) Invalid arguments!") : st

-- | > \[
builtinConcat :: BlsqState
builtinConcat = do
 st <- get
 case st of
  -- Special case for single char blocks
  (BlsqBlock [BlsqChar a] : xs) -> do putResult $ BlsqStr [a] : xs
  (BlsqBlock a) : xs -> do
     modify ((BlsqBlock $ [BlsqIdent "_+"]) :)
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

-- | > f[
builtinFilter :: BlsqState
builtinFilter = do
 st <- get
 case st of
  (BlsqBlock v : BlsqStr f : xs) -> do
      builtinSwap
      builtinExplode
      builtinSwap
      builtinFilter
      st' <- get
      case st' of
        (BlsqBlock a : xs) -> case null a of
                                True -> putResult $ BlsqStr "" : xs
                                False -> do builtinConcat
                                            boxString
        _ -> return ()
  (BlsqBlock f : BlsqBlock v : xs) -> do
    putResult $ (BlsqBlock $ filter' f v) : xs
  _ -> putResult $ BlsqError "Burlesque: (f[) Invalid arguments!" : st
 where filter' _ [] = []
       filter' f (x:xs) = case runStack f [x] of
                            (BlsqInt 0):ys -> filter' f xs
                            _ -> x : filter' f xs
       boxString = do
         st <- get
         case st of
          (BlsqChar a : xs) -> putResult $ BlsqStr [a] : xs
          _ -> return ()

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

-- | > E!
builtinEvalMany :: BlsqState
builtinEvalMany = do
 builtinMul
 builtinConcat
 builtinEval

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
   (BlsqStr a : xs) -> BlsqChar (maximum a) : xs
   (BlsqInt a : xs) -> BlsqInt (read . return . maximum . show $ a) : xs
   _ -> BlsqError "Burlesque: (>]) Invalid arguments!" : st

-- | > (<])
builtinMinimum :: BlsqState
builtinMinimum = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : xs) -> (minimum a) : xs
   (BlsqStr a : xs) -> BlsqChar (minimum a) : xs
   (BlsqInt a : xs) -> BlsqInt (read . return . minimum . show $ a) : xs
   _ -> BlsqError "Burlesque: (<]) Invalid arguments!" : st


-- | > (==)
builtinEqual :: BlsqState
builtinEqual = do
 st <- get
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a == b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (==) Invalid arguments!" : st

-- | > (!=)
builtinNotEqual :: BlsqState
builtinNotEqual = do
 builtinEqual
 builtinNot

-- | > (r_)
builtinRound :: BlsqState
builtinRound = do
 st <- get
 putResult $
  case st of
   (BlsqInt b : BlsqDouble a : xs) -> (BlsqDouble $ round' a b) : xs
   _ -> BlsqError "Burlesque: (r_) Invalid arguments!" : st
 where round' n s = let factor = fromIntegral (10^s) in fromIntegral (round (n * factor)) / factor

-- | R_
builtinRound2 :: BlsqState
builtinRound2 = do
 modify (BlsqInt 0 :)
 builtinRound
 builtinProduct

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
   (BlsqBlock a : xs) -> (BlsqBlock a) : xs
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
   (BlsqStr ls : xs) -> (BlsqBlock $ map (BlsqStr) (group ls)) : xs
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
   (BlsqInt 3 : BlsqPretty x _ : xs) -> (BlsqPretty x BlsqFormatRaw) : xs
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
   (BlsqInt a : xs) -> BlsqInt (a * a) : xs
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

-- | R@
builtinRangeInf :: BlsqState
builtinRangeInf = do
 st <- get
 putResult $
  case st of
   (BlsqStr a : xs) -> BlsqBlock (map BlsqStr $ subsequences a) : xs
   (BlsqBlock a : xs) -> BlsqBlock (map BlsqBlock $ subsequences a) : xs
   (BlsqInt a : xs) -> BlsqBlock (map BlsqInt [a..]) : xs
   _ -> BlsqError "Burlesque: (R@) Invalid arguments!" : st

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
   (BlsqInt a : xs) -> BlsqInt (read.sort.show.abs $ a) : xs
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

-- | > ZZ
builtinToUpper :: BlsqState
builtinToUpper = do
 st <- get
 putResult $
  case st of 
   (BlsqChar a : xs) -> BlsqChar (toUpper a) : xs
   (BlsqStr a : xs) -> BlsqStr (map toUpper a) : xs
   _ -> BlsqError "Burlesque: (ZZ) Invalid arguments!" : st

-- | > zz
builtinToLower :: BlsqState
builtinToLower = do
 st <- get
 putResult $
  case st of 
   (BlsqChar a : xs) -> BlsqChar (toLower a) : xs
   (BlsqStr a : xs) -> BlsqStr (map toLower a) : xs
   _ -> BlsqError "Burlesque: (zz) Invalid arguments!" : st

-- | > M[
builtinMapPretty :: BlsqState
builtinMapPretty = do
 builtinMap
 builtinPretty

-- | > M]
builtinMapToPretty :: BlsqState
builtinMapToPretty = do
 modify (BlsqBlock [BlsqIdent "sh"] : )
 builtinMap

-- | > m]
builtinMapToPrettyFromFormat :: BlsqState
builtinMapToPrettyFromFormat = do
 modify (BlsqBlock [BlsqIdent "sh", BlsqIdent "ff"] : )
 builtinMap

-- | > [m
builtinMapDup :: BlsqState
builtinMapDup = do
 modify (BlsqIdent "^^" :)
 builtinPrepend
 builtinMap

-- | > [M
builtinMapParse :: BlsqState
builtinMapParse = do
 modify (BlsqIdent "ps" :)
 builtinPrepend
 builtinMap

-- | ??
builtinVersion :: BlsqState
builtinVersion = modify (BlsqStr "Burlesque - 1.7 RC3" : )

-- | -~
builtinHeadTail :: BlsqState
builtinHeadTail = do
 builtinHead
 builtinTail

-- | \m
builtinConcatMap :: BlsqState
builtinConcatMap = do
 builtinMap
 builtinConcat

-- | wd
builtinWords :: BlsqState
builtinWords = do
 st <- get
 case st of
  (BlsqStr _ : xs) -> do
         modify (BlsqStr " " : )
         builtinSplit
  (BlsqBlock _ : xs) -> do
         modify (BlsqChar ' ' : )
         builtinSwap
         builtinIntersperse
         builtinConcat
  _ -> do builtinSwap
          builtinIntersperse

-- | z[
builtinZip :: BlsqState
builtinZip = do
 st <- get
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> (BlsqBlock $ map (\(x,y) -> BlsqBlock [x,y]) $ zip a b) : xs
   (BlsqStr b : BlsqStr a : xs) -> (BlsqBlock $ map (\(x,y) -> BlsqBlock [BlsqChar x,BlsqChar y]) $ zip a b) : xs
   (BlsqStr b : BlsqBlock a : xs) -> (BlsqBlock $ map (\(x,y) -> BlsqBlock [x,BlsqChar y]) $ zip a b) : xs
   (BlsqBlock b : BlsqStr a : xs) -> (BlsqBlock $ map (\(x,y) -> BlsqBlock [BlsqChar x,y]) $ zip a b) : xs
   _ -> BlsqError "Burlesque: (z[) Invalid arguments!" : st

-- | Z[
builtinZipWith :: BlsqState 
builtinZipWith = do
 st <- get
 case st of
   (BlsqBlock f : a : b : xs) -> do put $ a : b : BlsqBlock f : xs
                                    builtinZip
                                    builtinSwap
                                    builtinMap
                                    
-- | Z]
builtinZipWithPush :: BlsqState
builtinZipWithPush = do
 modify (BlsqIdent "^p" :)
 builtinPrepend
 builtinZipWith

-- | !!
builtinBlockAccess :: BlsqState
builtinBlockAccess = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : BlsqBlock b : xs) -> (b !! (toInt a)) : xs
   (BlsqInt a : BlsqStr b : xs) -> BlsqChar (b !! (toInt a)) : xs
   _ -> BlsqError "Burlesque: (!!) Invalid arguments!" : st

-- | fi
builtinFindIndex :: BlsqState
builtinFindIndex = do
  st <- get
  case st of
   (BlsqBlock p : BlsqBlock b : xs) -> putResult $ (BlsqInt $ findIndex' p b 0) : xs
   (BlsqBlock p : BlsqStr b : xs) -> builtinSwap >> builtinExplode >> 
                                     builtinSwap >> builtinFindIndex
   _ -> putResult $ BlsqError "Burlesque: (fi) Invalid arguments!" : st
 where findIndex' p [] _ = -1
       findIndex' p (x:xs) i = case runStack p [x] of
                                 (BlsqInt 1 : ys) -> i
                                 _ -> findIndex' p xs (succ i)

-- | Fi
builtinFindIndexEq :: BlsqState
builtinFindIndexEq = do
  st <- get
  case st of
   (p : BlsqBlock b : xs) -> putResult $ (BlsqInt $ findIndex' p b 0) : xs
   (BlsqChar p : BlsqStr b : xs) -> builtinSwap >> builtinExplode >> 
                                     builtinSwap >> builtinFindIndexEq
   _ -> putResult $ BlsqError "Burlesque: (fi) Invalid arguments!" : st
 where findIndex' p [] _ = -1
       findIndex' p (x:xs) i = case x == p of
                                 True -> i
                                 _ -> findIndex' p xs (succ i)

-- | fe
builtinFindElement :: BlsqState
builtinFindElement = do
  st <- get
  case st of
   (BlsqBlock p : BlsqBlock b : xs) -> putResult $ (findElement' p b) : xs
   (BlsqBlock p : BlsqStr b : xs) -> builtinSwap >> builtinExplode >> 
                                     builtinSwap >> builtinFindElement
   _ -> putResult $ BlsqError "Burlesque: (fi) Invalid arguments!" : st
 where findElement' p [] = BlsqError "Burlesque: (fe) Element not fund!"
       findElement' p (x:xs) = case runStack p [x] of
                                 (BlsqInt 1 : ys) -> x
                                 _ -> findElement' p xs

-- | CB
builtinCombinations :: BlsqState
builtinCombinations = do
 st <- get
 putResult $
  case st of
   (BlsqInt n : BlsqBlock ls : xs) -> (BlsqBlock $ map (BlsqBlock) (replicateM (toInt n) ls)) : xs
   (BlsqInt n : BlsqStr ls : xs) -> (BlsqBlock $ map (BlsqStr) (replicateM (toInt n) ls)) : xs
   (BlsqInt n : BlsqInt ls : xs) -> (BlsqBlock $ map (BlsqInt . read) (replicateM (toInt n) (show.abs $ ls))) : xs
   _ -> BlsqError "Burlesque: (CB) Invalid arguments!" : st

-- | cb
builtinCombinationsUpTo :: BlsqState
builtinCombinationsUpTo  = do
 st <- get
 putResult $
  case st of
   (BlsqInt n : BlsqBlock ls : xs) -> (BlsqBlock $ map (BlsqBlock) (combs (toInt n) ls)) : xs
   (BlsqInt n : BlsqStr ls : xs) -> (BlsqBlock $ map (BlsqStr) (combs (toInt n) ls)) : xs
   (BlsqInt n : BlsqInt ls : xs) -> (BlsqBlock $ map (BlsqInt . read) (combs (toInt n) (show.abs $ ls))) : xs
   _ -> BlsqError "Burlesque: (cb) Invalid arguments!" : st
 where combs n ls = enumFromTo 1 >=> flip replicateM ls $ n -- thanks to #haskell

-- | cy
builtinCycle :: BlsqState
builtinCycle = do
 st <- get
 putResult $
  case st of
   (BlsqBlock a : xs) -> (BlsqBlock (cycle a)) : xs
   (BlsqStr a : xs) -> (BlsqStr (cycle a)) : xs
   (BlsqInt a : xs) -> (BlsqStr (cycle . show . abs $ a)) : xs
   _ -> BlsqError "Burlesque: (cy) Invalid arguments!" : st

-- | is
builtinIsError :: BlsqState
builtinIsError = do
 st <- get
 putResult $
  case st of
   (BlsqError _ : xs) -> (BlsqInt 1) : xs
   _ -> (BlsqInt 0) : st

-- | fC
builtinPrimeFactors :: BlsqState
builtinPrimeFactors = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : xs) -> (BlsqBlock . map BlsqInt $ pfactor a) : xs
   (BlsqDouble a : xs) -> (BlsqDouble $ a * a) : xs
   _ -> BlsqError "Burlesque: (fC) Invalid arguments" : st

-- | fc
builtinFactors :: BlsqState
builtinFactors = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : xs) -> (BlsqBlock $ map (BlsqInt) $ factors' a a) : xs
   (BlsqStr a : xs) -> (BlsqChar $ leastCommon a) : xs
   (BlsqBlock a : xs) -> (leastCommon a) : xs

   _ -> BlsqError "Burlesque: (fc) Invalid arguments!" : st
 where factors' _ 0 = []
       factors' a b
         |a `rem` b == 0 = b : factors' a (b - 1)
         |otherwise = factors' a (b - 1)
       leastCommon :: Ord a => [a] -> a
       leastCommon = head . minimumBy (comparing length) . group . sort

-- | co
builtinChunksOf :: BlsqState
builtinChunksOf = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : BlsqBlock ls : xs) -> BlsqBlock (map BlsqBlock $ chunksOf' a ls) : xs
   (BlsqInt a : BlsqStr ls : xs) -> BlsqBlock (map BlsqStr $ chunksOf' a ls) : xs
   (BlsqInt a : BlsqInt ls : xs) -> BlsqBlock (map (BlsqInt . read) $ chunksOf' a (show . abs $ ls)) : xs
   _ -> BlsqError "Burlesque: (co) Invalid arguments!" : st
 where chunksOf' _ [] = []
       chunksOf' n xs = genericTake n xs : chunksOf' n (genericDrop n xs)

-- | CO
builtinChunky :: BlsqState
builtinChunky = do
 st <- get
 putResult $
  case st of
   (BlsqInt a : BlsqBlock ls : xs) -> BlsqBlock (map BlsqBlock $ chunky' a ls) : xs
   (BlsqInt a : BlsqStr ls : xs) -> BlsqBlock (map BlsqStr $ chunky' a ls) : xs
   (BlsqInt a : BlsqInt ls : xs) -> BlsqBlock (map (BlsqInt . read) $ chunky' a (show . abs $ ls)) : xs
   _ -> BlsqError "Burlesque: (co) Invalid arguments!" : st
 where chunky' n = takeWhile ((==n).genericLength) . map (genericTake n) . tails

-- | t[
builtinTrimLeft :: BlsqState
builtinTrimLeft = do
 st <- get
 putResult $
  case st of
   (BlsqStr a : xs) -> BlsqStr (dropWhile (`elem`"\t \n") a) : xs
   _ -> BlsqChar '\n' : st

-- | t]
builtinTrimRight :: BlsqState
builtinTrimRight = do
 st <- get
 putResult $
  case st of
   (BlsqStr a : xs) -> BlsqStr (reverse (dropWhile (`elem`"\t \n") (reverse a))) : xs
   _ -> BlsqChar '\'' : st

builtinTrimLeftRight :: BlsqState
builtinTrimLeftRight = do
 builtinTrimLeft
 builtinTrimRight

-- | n!
builtinNot :: BlsqState
builtinNot = do
 st <- get
 putResult $
  case st of
   (BlsqInt 0 : xs) -> BlsqInt 1 : xs
   (BlsqInt _ : xs) -> BlsqInt 0 : xs
   (BlsqStr a : xs) -> (BlsqChar $ mostCommon a) : xs
   (BlsqBlock a : xs) -> (mostCommon a) : xs
   _ -> BlsqError "Burlsque: (n!) Invalid arguments!" : st
 where mostCommon :: Ord a => [a] -> a
       mostCommon = head . maximumBy (comparing length) . group . sort


-- | fI
builtinFindIndices :: BlsqState
builtinFindIndices = do
  st <- get
  case st of
   (BlsqBlock p : BlsqBlock b : xs) -> putResult $ BlsqBlock (map BlsqInt $ findIndices' p b 0) : xs
   (BlsqBlock p : BlsqStr b : xs) -> builtinSwap >> builtinExplode >> 
                                     builtinSwap >> builtinFindIndices
   _ -> putResult $ BlsqError "Burlesque: (fi) Invalid arguments!" : st
 where findIndices' p [] _ = []
       findIndices' p (x:xs) i = case runStack p [x] of
                                 (BlsqInt 1 : ys) -> i : findIndices' p xs (succ i)
                                 _ -> findIndices' p xs (succ i) 

-- | lg
builtinLog :: BlsqState
builtinLog = do
  st <- get
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (log (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (log a) : xs

-- | LG
builtinLog2 :: BlsqState
builtinLog2 = do
  builtinLog 
  builtinSwap
  builtinLog
  builtinDiv

-- | Ts
builtinSin :: BlsqState
builtinSin = do
  st <- get
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (sin (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (sin a) : xs
   _ -> putResult $ BlsqError "Burlesque: (Ts) Invalid arguments!" : st

-- | TS
builtinAsin :: BlsqState
builtinAsin = do
  st <- get
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (asin (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (asin a) : xs
   _ -> putResult $ BlsqError "Burlesque: (TS) Invalid arguments!" : st

-- | Tc
builtinCos :: BlsqState
builtinCos = do
  st <- get
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (cos (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (cos a) : xs
   _ -> putResult $ BlsqError "Burlesque: (Tc) Invalid arguments!" : st

-- | TC
builtinAcos :: BlsqState
builtinAcos = do
  st <- get
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (acos (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (acos a) : xs
   _ -> putResult $ BlsqError "Burlesque: (TC) Invalid arguments!" : st

-- | Tt
builtinTan :: BlsqState
builtinTan = do
  st <- get
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (tan (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (tan a) : xs
   _ -> putResult $ BlsqError "Burlesque: (Tt) Invalid arguments!" : st

-- | TT
builtinAtan :: BlsqState
builtinAtan = do
  st <- get
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (atan (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (atan a) : xs
   _ -> putResult $ BlsqError "Burlesque: (TT) Invalid arguments!" : st

-- | WD
builtinWords2 :: BlsqState
builtinWords2 = do
  st <- get
  case st of
    (BlsqStr a : xs) -> putResult $ (BlsqBlock $ map BlsqStr (words a)) : xs
    _ -> putResult $ BlsqError "Burlesque: (WD) Invalid arguments!" : st

-- | wD
builtinWords3 :: BlsqState
builtinWords3 = do
 builtinWords
 builtinPretty

-- | ia
builtinInsertAt :: BlsqState
builtinInsertAt = do
  st <- get
  case st of
    (BlsqInt idx : e : BlsqBlock ls : xs) -> putResult $ (BlsqBlock (Burlesque.Helpers.insertAt idx e ls)) : xs
    (BlsqInt idx : BlsqChar e : BlsqStr ls : xs) -> putResult $ (BlsqStr (Burlesque.Helpers.insertAt idx e ls)) : xs
    _ -> putResult $ BlsqError "Burlesque: (ia) Invalid arguments!" : st

-- | RA
builtinRemoveAt :: BlsqState
builtinRemoveAt = do
  st <- get
  case st of
    (BlsqInt idx : BlsqBlock ls : xs) -> putResult $ (BlsqBlock (removeAt idx ls)) : xs
    (BlsqInt idx : BlsqStr ls : xs) -> putResult $ (BlsqStr (removeAt idx ls)) : xs
    (BlsqBlock ls : xs) -> do
          builtinSetAt
          builtinProduct
          modify (BlsqDouble 2.0 : )
          builtinDiv
          builtinAverage
          builtinBlockAccess
    _ -> putResult $ BlsqError "Burlesque: (ra) Invalid arguments!" : st

-- | sa
builtinSetAt :: BlsqState
builtinSetAt = do
  st <- get
  case st of
    (BlsqInt idx : e : BlsqBlock ls : xs) -> putResult $ (BlsqBlock (setAt idx e ls)) : xs
    (BlsqInt idx : BlsqChar e : BlsqStr ls : xs) -> putResult $ (BlsqStr (setAt idx e ls)) : xs
    (BlsqBlock ls : xs) -> do 
          builtinDup
          builtinLength
    (BlsqStr ls : xs) -> do
          builtinDup
          builtinLength
    _ -> putResult $ BlsqError "Burlesque: (sa) Invalid arguments!" : st 

-- | sb
builtinSortBy :: BlsqState
builtinSortBy = do
  st <- get
  case st of
    (BlsqBlock f : BlsqBlock ls : xs) -> putResult $ BlsqBlock (
                                         sortBy (\ a b -> case runStack f [b,a] of
                                                                 (BlsqInt 1 : xs) -> GT
                                                                 (BlsqInt (-1) : xs) -> LT
                                                                 _ -> EQ) ls) : xs
    (BlsqBlock f : BlsqStr ls : xs) -> putResult $ BlsqStr (
                                       sortBy (\ a b -> case runStack f [BlsqChar b,BlsqChar a] of
                                                                 (BlsqInt 1 : xs) -> GT
                                                                 (BlsqInt (-1) : xs) -> LT
                                                                 _ -> EQ) ls) : xs
    _ -> putResult $ BlsqError "Burlesque: (sb) Invalid arguments!" : st

-- | cm
builtinCompare :: BlsqState
builtinCompare = do
  st <- get
  case st of
    (b : a : xs) -> putResult $ BlsqInt (case compare a b of
                                           GT -> 1
                                           LT -> -1
                                           EQ -> 0) : xs
    _ -> putResult $ BlsqError "Burlesque: (cm) Invalid arguments!" : st

-- | CM
builtinCompare2 :: BlsqState
builtinCompare2 = do
  builtinDup
  builtinBox
  modify (BlsqIdent "\\/" :)
  builtinAppend
  builtinSwap
  builtinAppend
  modify (BlsqIdent "\\/" :)
  builtinAppend
  modify (BlsqIdent "cm" :)
  builtinAppend
  
-- | Cm
builtinCompare3 :: BlsqState
builtinCompare3 = do
  builtinDup
  modify (BlsqIdent "\\/" :)
  builtinAppend
  builtinSwap
  builtinHead
  builtinAppend
  modify (BlsqIdent "\\/" :)
  builtinAppend
  modify (BlsqIdent "cm" :)
  builtinAppend

-- | B!
builtinConvertBase :: BlsqState
builtinConvertBase = do
  st <- get
  case st of
    (BlsqInt bs : BlsqInt n : xs) -> putResult $ BlsqStr (toBase (fromIntegral bs) (fromIntegral n)) : xs
    (BlsqInt bs : BlsqStr n : xs) -> putResult $ BlsqInt (toInteger (fromBase (fromIntegral bs) n)) : xs
    _ -> putResult $ BlsqError "Burlesque: (B!) Invalid arguments!" : st

-- | g_
builtinGcd :: BlsqState
builtinGcd = do
  st <- get
  case st of
    (BlsqInt b : BlsqInt a : xs) -> putResult $ BlsqInt (gcd a b) : xs
    (BlsqBlock ls : xs) -> do builtinDup
                              builtinHead
                              builtinSwap
                              builtinTail
                              builtinSwap
    (BlsqStr ls : xs) -> do builtinDup
                            builtinHead
                            builtinSwap
                            builtinTail
                            builtinSwap
    _ -> putResult $ BlsqError "Burlesque: (g_) Invalid arguments!" : st

-- | l_
builtinLcm :: BlsqState
builtinLcm = do
  st <- get
  case st of
    (BlsqInt b : BlsqInt a : xs) -> putResult $ BlsqInt (lcm a b) : xs
    (BlsqBlock ls : xs) -> do builtinDup
                              builtinInit
                              builtinSwap
                              builtinLast
                              builtinSwap
    (BlsqStr ls : xs) -> do builtinDup
                            builtinInit
                            builtinSwap
                            builtinLast
                            builtinSwap
    _ -> putResult $ BlsqError "Burlesque: (l_) Invalid arguments!" : st
	
-- |tw
builtinTakeWhile :: BlsqState
builtinTakeWhile = do
 st <- get
 case st of
   (BlsqBlock ls : BlsqBlock p : xs) -> putResult $ (BlsqBlock $ takeWhile' p ls) : xs
   (BlsqStr ls : BlsqBlock p : xs) -> do builtinExplode
                                         builtinTakeWhile
                                         builtinConcat
   _ -> putResult $ BlsqError "Burlesque: (tw) Invalid arguments!" : st
 where takeWhile' _ [] = []
       takeWhile' p (y:ys) = case runStack p [y] of
                               (BlsqInt 0 : _) -> []
                               _ -> y : takeWhile' p ys

-- |dw
builtinDropWhile :: BlsqState
builtinDropWhile = do
 st <- get
 case st of
   (BlsqBlock ls : BlsqBlock p : xs) -> putResult $ (BlsqBlock $ dropWhile' p ls) : xs
   (BlsqStr ls : BlsqBlock p : xs) -> do builtinExplode
                                         builtinDropWhile
                                         builtinConcat
   _ -> putResult $ BlsqError "Burlesque: (dw) Invalid arguments!" : st
 where dropWhile' _ [] = []
       dropWhile' p yss@(y:ys) = case runStack p [y] of
                               (BlsqInt 0 : _) -> yss
                               _ -> dropWhile' p ys

-- | tp
builtinTranspose :: BlsqState
builtinTranspose = do 
 st <- get
 case st of
  (BlsqBlock a : xs) -> putResult $ BlsqBlock (map BlsqBlock (transpose (map (toList') a))) : xs
  _ -> putResult $  BlsqError "You should not transpose what you can't transpose. Yes this is an easteregg!" : st
 where toList' (BlsqBlock a) = a
       toList' x = [x]

-- | FM
builtinFilterMap :: BlsqState
builtinFilterMap = do
 st <- get
 case st of
  (BlsqBlock m : BlsqBlock f : xs) -> do builtinPop
                                         builtinPop
                                         modify (BlsqBlock f :)
                                         builtinFilter
                                         modify (BlsqBlock m :)
                                         builtinMap
  _ -> putResult $ BlsqError "Burlesque: (FM) Invalid arguments!" : st

-- | r\
builtinRangeConcat :: BlsqState
builtinRangeConcat = do builtinRange
                        builtinConcat

-- | SP
builtinSpecialInput :: BlsqState
builtinSpecialInput = do 
 st <- get
 case st of
  (BlsqStr s : xs) -> do builtinLines
                         modify (BlsqBlock [] :)
                         builtinMapParse
  (BlsqBlock l : xs) -> do modify (BlsqBlock [ BlsqBlock [ BlsqIdent "Sh"], BlsqIdent "m[", BlsqIdent "wd" ] :)
                           builtinMap
                           builtinUnlines
  _ -> putResult $ BlsqError "Burlesque: (SP) Invalid arguments!" : st

-- | hd
-- Unlike high definition this is a gruesome hack. or feature. let's say feature.
builtinHide :: BlsqState
builtinHide = do
 st <- get
 case st of
   (a : xs) -> do builtinPop
                  modify (++[BlsqHiddenState a])
   _ -> putResult $ BlsqError "Burlesque: (hd) Invalid arguments!" : st

-- | HD
-- also hackish.
builtinHide2 :: BlsqState
builtinHide2 = do
 st <- get
 case st of
   (a : xs) -> do builtinPop
                  modify (BlsqHiddenState a : )
   _ -> putResult $ BlsqError "Burlesque: (hd) Invalid arguments!" : st
   
-- | ld
-- very hackish
builtinLoad :: BlsqState
builtinLoad = do
 st <- get
 case st of
   (BlsqInt a : xs) -> do builtinPop
                          let hidden = st !! (toInt (length st - 1 - (toInt a)))
                          case hidden of
                            (BlsqHiddenState hstate) -> do modify(hstate :)
                            _ -> do modify (BlsqError "Can't load non hidden state! Sorry." :)
   _ -> putResult $ BlsqError "Burlesque: (ld) Invalid arguments!" : st

-- | LD
builtinLoad2 :: BlsqState
builtinLoad2 = do builtinLoad
                  builtinEval

-- | st
builtinStore :: BlsqState
builtinStore = do
 st <- get
 case st of
   (BlsqInt a : e : xs) -> do put $ setAt (toInt (length xs - 1 - (toInt a))) (BlsqHiddenState e) xs
   _ -> putResult $ BlsqError "Burlesque: (ld) Invalid arguments!" : st

builtinALoad = modify (BlsqInt 0 :) >> builtinLoad
builtinBLoad = modify (BlsqInt 1 :) >> builtinLoad
builtinCLoad = modify (BlsqInt 2 :) >> builtinLoad
builtinALoad2 = modify (BlsqInt 0 :) >> builtinLoad2
builtinBLoad2 = modify (BlsqInt 1 :) >> builtinLoad2
builtinCLoad2 = modify (BlsqInt 2 :) >> builtinLoad2
builtinAStore = modify (BlsqInt 0 :) >> builtinStore
builtinBStore = modify (BlsqInt 1 :) >> builtinStore
builtinCStore = modify (BlsqInt 2 :) >> builtinStore

-- | sc
builtinSortByComparing :: BlsqState
builtinSortByComparing = do
 builtinCompare2
 builtinSortBy

-- | ?+
builtinCoerceAdd :: BlsqState
builtinCoerceAdd = do
 st <- get
 case st of
   (BlsqInt a : BlsqDouble b : xs) -> do builtinProduct
                                         builtinAdd
   (BlsqDouble a : BlsqInt b : xs) -> do builtinSwap
                                         builtinProduct
                                         builtinSwap
                                         builtinAdd
   (BlsqStr a : BlsqInt b : xs) -> do builtinSwap
                                      builtinPrettyFromFormat
                                      builtinSwap
                                      builtinAdd
   (BlsqInt a : BlsqStr b : xs) -> do builtinPrettyFromFormat
                                      builtinAdd
   (BlsqStr a : BlsqDouble b : xs) -> do builtinSwap
                                         builtinPrettyFromFormat
                                         builtinSwap
                                         builtinAdd
   (BlsqDouble a : BlsqStr b : xs) -> do builtinPrettyFromFormat
                                         builtinAdd
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoerceAdd
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoerceAdd
   (BlsqBlock a : BlsqDouble b : xs) -> do builtinSwap
                                           builtinBox
                                           builtinCycle
                                           builtinSwap
                                           builtinCoerceAdd
   (BlsqDouble a : BlsqBlock b : xs) -> do builtinBox
                                           builtinCycle
                                           builtinCoerceAdd
   (BlsqBlock a : BlsqStr b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoerceAdd
   (BlsqStr a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoerceAdd
   (BlsqBlock a : BlsqBlock b : xs) -> do modify (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?+" ] : )
                                          builtinZipWith
   _ -> builtinAdd

-- | ?-
builtinCoerceSub :: BlsqState
builtinCoerceSub = do
 st <- get
 case st of
   (BlsqInt a : BlsqDouble b : xs) -> do builtinProduct
                                         builtinSub
   (BlsqDouble a : BlsqInt b : xs) -> do builtinSwap
                                         builtinProduct
                                         builtinSwap
                                         builtinSub
   (BlsqStr a : BlsqInt b : xs) -> do builtinSwap
                                      builtinPrettyFromFormat
                                      builtinSwap
                                      builtinSub
   (BlsqInt a : BlsqStr b : xs) -> do builtinPrettyFromFormat
                                      builtinSub
   (BlsqStr a : BlsqDouble b : xs) -> do builtinSwap
                                         builtinPrettyFromFormat
                                         builtinSwap
                                         builtinSub
   (BlsqDouble a : BlsqStr b : xs) -> do builtinPrettyFromFormat
                                         builtinSub
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoerceSub
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoerceSub
   (BlsqBlock a : BlsqDouble b : xs) -> do builtinSwap
                                           builtinBox
                                           builtinCycle
                                           builtinSwap
                                           builtinCoerceSub
   (BlsqDouble a : BlsqBlock b : xs) -> do builtinBox
                                           builtinCycle
                                           builtinCoerceSub
   (BlsqBlock a : BlsqStr b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoerceSub
   (BlsqStr a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoerceSub
   (BlsqBlock a : BlsqBlock b : xs) -> do modify (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?-" ] : )
                                          builtinZipWith
   _ -> builtinSub

-- | ?/
builtinCoerceDiv :: BlsqState
builtinCoerceDiv = do
 st <- get
 case st of
   (BlsqInt a : BlsqDouble b : xs) -> do builtinProduct
                                         builtinDiv
   (BlsqDouble a : BlsqInt b : xs) -> do builtinSwap
                                         builtinProduct
                                         builtinSwap
                                         builtinDiv
   (BlsqStr a : BlsqInt b : xs) -> do builtinSwap
                                      builtinPrettyFromFormat
                                      builtinSwap
                                      builtinDiv
   (BlsqInt a : BlsqStr b : xs) -> do builtinPrettyFromFormat
                                      builtinDiv
   (BlsqStr a : BlsqDouble b : xs) -> do builtinSwap
                                         builtinPrettyFromFormat
                                         builtinSwap
                                         builtinDiv
   (BlsqDouble a : BlsqStr b : xs) -> do builtinPrettyFromFormat
                                         builtinDiv
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoerceDiv
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoerceDiv
   (BlsqBlock a : BlsqDouble b : xs) -> do builtinSwap
                                           builtinBox
                                           builtinCycle
                                           builtinSwap
                                           builtinCoerceDiv
   (BlsqDouble a : BlsqBlock b : xs) -> do builtinBox
                                           builtinCycle
                                           builtinCoerceDiv
   (BlsqBlock a : BlsqStr b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoerceDiv
   (BlsqStr a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoerceDiv
   (BlsqBlock a : BlsqBlock b : xs) -> do modify (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?/" ] : )
                                          builtinZipWith
   _ -> builtinDiv

-- | ?*
builtinCoerceMul :: BlsqState
builtinCoerceMul = do
 st <- get
 case st of
   (BlsqInt a : BlsqDouble b : xs) -> do builtinProduct
                                         builtinMul
   (BlsqDouble a : BlsqInt b : xs) -> do builtinSwap
                                         builtinProduct
                                         builtinSwap
                                         builtinMul
   (BlsqStr a : BlsqInt b : xs) -> do builtinSwap
                                      builtinPrettyFromFormat
                                      builtinSwap
                                      builtinMul
   (BlsqInt a : BlsqStr b : xs) -> do builtinPrettyFromFormat
                                      builtinMul
   (BlsqStr a : BlsqDouble b : xs) -> do builtinSwap
                                         builtinPrettyFromFormat
                                         builtinSwap
                                         builtinMul
   (BlsqDouble a : BlsqStr b : xs) -> do builtinPrettyFromFormat
                                         builtinMul
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoerceMul
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoerceMul
   (BlsqBlock a : BlsqDouble b : xs) -> do builtinSwap
                                           builtinBox
                                           builtinCycle
                                           builtinSwap
                                           builtinCoerceMul
   (BlsqDouble a : BlsqBlock b : xs) -> do builtinBox
                                           builtinCycle
                                           builtinCoerceMul
   (BlsqBlock a : BlsqStr b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoerceMul
   (BlsqStr a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoerceMul
   (BlsqBlock a : BlsqBlock b : xs) -> do modify (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?*" ] : )
                                          builtinZipWith
   _ -> builtinMul
   
-- | im
builtinImplode :: BlsqState
builtinImplode = do 
 modify (BlsqBlock [BlsqIdent "++"] :)
 builtinReduce
 
-- | ms
builtinMapSum :: BlsqState
builtinMapSum = do
 builtinMap
 builtinSum
 
-- | ms
builtinMapProduct :: BlsqState
builtinMapProduct = do
 builtinMap
 builtinProduct

-- | sg
builtinSortGroup :: BlsqState
builtinSortGroup = do
 builtinSort
 builtinGroup
 
-- | gs
builtinGroupSort :: BlsqState
builtinGroupSort = do
 builtinGroup
 builtinSort
 
-- | sH
builtinPrettyPretty :: BlsqState
builtinPrettyPretty = do
 builtinPrettyFormatFromFormat
 builtinPretty
 
-- | cp
builtinCrossProduct :: BlsqState
builtinCrossProduct = do
 st <- get
 case st of
   (BlsqBlock b : BlsqBlock a : xs) -> putResult $ BlsqBlock [BlsqBlock [x,y] | x <- a, y <- b] : xs
   (BlsqStr b : BlsqStr a : xs) -> putResult $ BlsqBlock [BlsqStr [x,y] | x <- a, y <- b] : xs
   _ -> putResult $ BlsqError "Burlesque: (cp) Invalid arguments!" : st
   
-- | bc
builtinBoxCycle :: BlsqState
builtinBoxCycle = do
 builtinBox
 builtinCycle
 
-- | rt
builtinRotate :: BlsqState
builtinRotate = do
 builtinLcm
 builtinSwap
 builtinPrepend
 
-- | RT
builtinRotate2 :: BlsqState
builtinRotate2 = do
 builtinGcd
 builtinAppend
 
-- |d!
builtinDimArrayAccess :: BlsqState
builtinDimArrayAccess = do
 modify (BlsqIdent "!!" :)
 builtinSwap
 builtinIntersperse
 modify (BlsqIdent "!!" :)
 builtinAppend
 builtinEval
 
-- | D!
builtinDimArraySet :: BlsqState
builtinDimArraySet = do
 --{0 1 1} -> ^^0!!^^1!!8 1sa1 sa0 sa
 st <- get
 case st of
  (e : BlsqBlock adr : BlsqBlock arr : xs) -> do
    let f = concatMap(\c -> [BlsqIdent "^^",c,BlsqIdent "!!"]) $ init adr
    let f' = f++[e]
    let g = concatMap(\c -> [c,BlsqIdent "sa"]) $ reverse adr
    putResult $ BlsqBlock (f' ++ g) : BlsqBlock arr : xs
    builtinEval
  _ -> putResult $ BlsqError "Burlesque: (D!) Invalid arguments!" : st

-- | ]m
builtinMapString :: BlsqState
builtinMapString = do
 --{*Sh}m[
 modify(BlsqIdent "Sh": )
 builtinAppend
 builtinMap
 
-- | Wl
builtinWithLinesString :: BlsqState
builtinWithLinesString = do
 modify(BlsqIdent "Sh": )
 builtinAppend
 builtinWithLinesPretty
 
-- | si
builtinSelectIndices :: BlsqState
builtinSelectIndices = do
 -- {1 0 3}{"ABCD"\/!!}m[
 st <- get
 case st of
  (BlsqBlock adr : BlsqBlock ls : xs) -> do
    let f = BlsqBlock [ BlsqBlock ls, BlsqIdent "\\/", BlsqIdent "!!" ]
    putResult $ f : BlsqBlock adr : xs
    builtinMap
  (BlsqBlock adr : BlsqStr ls : xs) -> do
    builtinSwap
    builtinExplode
    builtinSwap
    builtinSelectIndices
    builtinConcat
  _ -> putResult $ BlsqError "Burlesque: (si) Invalid arguments!" : st
  
-- | ro
builtinRangeFromOne :: BlsqState
builtinRangeFromOne = do
 modify (BlsqInt 1 :)
 builtinSwap
 builtinRange
 
-- | rz
builtinRangeFromZero :: BlsqState
builtinRangeFromZero = do
 modify (BlsqInt 0 :)
 builtinSwap
 builtinRange
 
-- | <>
builtinSortReverse :: BlsqState
builtinSortReverse = do
 builtinSort
 builtinReverse
 
-- | nu
builtinNull :: BlsqState
builtinNull = do
 builtinLength
 builtinNot
 
-- | fl
builtinFilterLength :: BlsqState
builtinFilterLength = do
 builtinFilter
 builtinLength
 
-- | to
builtinTypeOf :: BlsqState
builtinTypeOf = do
 st <- get
 putResult $
  case st of
   (BlsqInt _ : xs) -> BlsqStr "Int" : xs
   (BlsqDouble _ : xs) -> BlsqStr "Double" : xs
   (BlsqStr _ : xs) -> BlsqStr "Str" : xs
   (BlsqBlock _ : xs) -> BlsqStr "Block" : xs
   (BlsqPretty _ _ : xs) -> BlsqStr "Pretty" : xs
   (BlsqIdent _ : xs) -> BlsqStr "Ident" : xs
   (BlsqNil : xs) -> BlsqStr "Nil" : xs
   (BlsqHiddenState _ : xs) -> BlsqStr "HiddenState" : xs
   (BlsqQuoted _ : xs) -> BlsqStr "Quoted" : xs
   (BlsqSpecial _ : xs) -> BlsqStr "Special" : xs
   (BlsqError _ : xs) -> BlsqStr "Error" : xs
   (BlsqHackMode _ :xs) -> BlsqStr "HackMode" : xs
   (BlsqChar _ : xs) -> BlsqStr "Char" : xs
   _ -> BlsqStr "Dafuq? You found a type I don't know?" : st
   
-- | sr
builtinSplitRegex :: BlsqState
builtinSplitRegex = do
 st <- get
 putResult $
  case st of
   (BlsqStr s : BlsqStr rgx : xs) -> BlsqBlock (map BlsqStr . splitRegex (mkRegex rgx) $ s) : xs
   _ -> BlsqError "Burlesque: (sr) Invalid arguments!" : st
   
-- | rn
builtinRandomInts :: BlsqState
builtinRandomInts = do
 st <- get
 putResult $
  case st of
   (BlsqInt ho : BlsqInt lo : BlsqInt seed : xs) -> (BlsqBlock . map BlsqInt $ randomRs (lo,ho) (mkStdGen (toInt seed))) : xs
   _ -> BlsqError "Burlesque: (rn) Invalid arguments!" : st
   
-- | RN
builtinRandomDoubles :: BlsqState
builtinRandomDoubles = do
 st <- get
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lo : BlsqInt seed : xs) -> (BlsqBlock . map BlsqDouble $ randomRs (lo,ho) (mkStdGen (toInt seed))) : xs
   _ -> BlsqError "Burlesque: (RN) Invalid arguments!" : st
   
-- | <m
builtinMinimumBy :: BlsqState
builtinMinimumBy = do
 builtinCompare3
 builtinSortBy
 builtinHead
 
-- | >m
builtinMaximumBy :: BlsqState
builtinMaximumBy = do
 builtinCompare3
 builtinSortBy
 builtinReverse
 builtinHead

-- | > gr
builtinGrep :: BlsqState
builtinGrep = do
 -- {~=}\/+]\/ln\/f[un
 modify (BlsqBlock [BlsqIdent "~="] :)
 builtinSwap
 builtinPrepend
 builtinSwap
 builtinLines
 builtinSwap
 builtinFilter
 builtinUnlines
 
-- | pm
builtinPlusMinus :: BlsqState
builtinPlusMinus = do
 builtinAdd
 builtinSub
 
-- | nc
builtinNormalDCumulative :: BlsqState
builtinNormalDCumulative = do
 st <- get
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble std : BlsqDouble mn : xs) -> BlsqDouble (cumulative (normalDistr mn std) ho) : xs
   _ -> BlsqError "Burlesque: (nc) Invalid arguments!" : st
   
-- | nd
builtinNormalDDensity :: BlsqState
builtinNormalDDensity = do
 st <- get
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble std : BlsqDouble mn : xs) -> BlsqDouble (density (normalDistr mn std) ho) : xs
   _ -> BlsqError "Burlesque: (nd) Invalid arguments!" : st
   
-- | Bc
builtinBinomialDCumulative :: BlsqState
builtinBinomialDCumulative = do
 st <- get
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble p : BlsqInt n : xs) -> BlsqDouble (cumulative (binomial (toInt n) p) ho) : xs
   _ -> BlsqError "Burlesque: (Bc) Invalid arguments!" : st
   
-- | Bp
builtinBinomialDProbability :: BlsqState
builtinBinomialDProbability = do
 st <- get
 putResult $
  case st of
   (BlsqInt ho : BlsqDouble p : BlsqInt n : xs) -> BlsqDouble (probability (binomial (toInt n) p) (toInt ho)) : xs
   _ -> BlsqError "Burlesque: (Bp) Invalid arguments!" : st
   
-- | pc
builtinPoissonDCumulative :: BlsqState
builtinPoissonDCumulative = do
 st <- get
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lambda :xs) -> BlsqDouble (cumulative (poisson (lambda)) ho) : xs
   _ -> BlsqError "Burlesque: (pc) Invalid arguments!" : st
   
-- | pp
builtinPoissonDProbability :: BlsqState
builtinPoissonDProbability = do
 st <- get
 putResult $
  case st of
   (BlsqInt ho : BlsqDouble lambda : xs) -> BlsqDouble (probability (poisson (lambda)) (toInt ho)) : xs
   _ -> BlsqError "Burlesque: (pp) Invalid arguments!" : st