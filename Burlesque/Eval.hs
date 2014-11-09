module Burlesque.Eval
  (eval, runStack, builtins, fst')
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
import Numeric
import Control.Monad
import System.Random
import Data.Digits
import Data.Function

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

import qualified Data.Map as M

import Debug.Trace

-- | > Evaluate a Burlesque program
eval :: BlsqProg -> BlsqState
eval (BlsqSpecial "@" : BlsqIdent s : xs) = do
 pushToStack (BlsqChar $ s !! 0)
 pushToStack (BlsqChar $ s !! 1)
 eval xs
eval (BlsqSpecial "@" : BlsqChar c : xs) = do
 pushToStack (BlsqStr $ [c])
 builtinCycle
 eval xs
eval (BlsqSpecial "@" : BlsqInt i : xs) = do
 pushToStack (BlsqDouble $ fromIntegral i)
 eval xs
eval (BlsqSpecial ")" : i : xs) = do
 pushToStack (BlsqBlock [ i ])
 builtinMap
 eval xs
eval (BlsqSpecial ":" : i : xs) = do
 pushToStack (BlsqBlock [ i ])
 builtinFilter
 eval xs
eval (BlsqIdent "#Q" : xs) = do
 pushToStack (BlsqBlock xs)
 eval xs
eval (BlsqIdent "#q" : xs) = do
 st <- getStack
 case st of
   (BlsqBlock nss : ss) -> builtinPop >> eval nss
   _ -> eval xs
eval (BlsqIdent "#j" : xs) = do
 st <- getStack
 case st of
   (BlsqBlock nss : ss) -> builtinPop >> eval (nss++xs)
   _ -> eval xs
eval (BlsqIdent "#J" : xs) = do
 st <- getStack
 case st of
   (BlsqBlock nss : ss) -> builtinPop >> eval (xs++nss)
   _ -> eval xs
eval (BlsqHackMode x : xs) = do
 let m = map (\c -> BlsqIdent . fst $ builtins !! (ord c)) x
 eval (m ++ xs)
 
eval (x:xs) = evalI x >> eval xs
eval [] = return ()

evalI (BlsqQuoted q) = pushToStack q
evalI v@(BlsqSpecial ",") = do
 st <- getStack
 if length st == 1 then
   do putStack []
 else return ()
evalI v@(BlsqIdent i) = lookupBuiltin i
evalI (BlsqMapBlock e) = do
  pushToStack (BlsqBlock e)
  builtinMap
evalI (BlsqAssign name e) = do
  (st, st', v) <- get
  let v' = M.insert (BlsqStr name) e v
  put (st, st', v')
evalI (BlsqCall name) = do
  pushToStack (BlsqStr name)
  builtinGetVar
  builtinEval
evalI v = pushToStack v

-- | > Run program with empty stack
--run :: BlsqProg -> BlsqStack
--run p = fst $ execState (runStack p []) ([],[], M.fromList [])

putStack q = do
  (_ , st', v') <- get
  put (q, st', v')

getStack = do
  (st, _, _) <- get
  return st
  
pushStateStack q = do
  (st, st', v') <- get
  put (st, q:st', v')
  
popStateStack = do
  (st, (s:st'), v') <- get
  put (st, st', v')
  return s
  
swapStateStack = do
  (st, (a:b:st'), v') <- get
  put (st,b:a:st', v')
  
pushToStack q = do
  (st, st', v') <- get
  put (q:st, st', v')
  
popFromStack = do
  (s:st, st', v') <- get
  put (st, st', v')
  return s
  
pushToBottom q = do
  (st, st', v') <- get
  put (st++[q], st', v')
  
setVar name value = do
  (st, st', v) <- get
  put (st, st', M.insert name value v)
  
getVar name  = do
  (_, _, v) <- get
  return $ fromMaybe BlsqNil (M.lookup name v)
  
fst' (a,b,c) = a
snd' (a,b,c) = b
trd'  (a,b,c) = c
  
-- | > Run program with predefined stack
runStack :: BlsqProg -> BlsqStack -> BlsqState' BlsqStack
runStack p xs = do
  (st, st', v') <- get
  put (xs, st', v')
  eval p
  (nst, st'', v'') <- get
  put (st, st'', v'')
  return nst
  
runStack' :: BlsqProg -> BlsqStack -> BlsqStack
runStack' p xs = fst' $ execState (eval p) (xs,[],M.fromList [])

runStack'' p xs g v = execState (eval p) (xs,g, v)

toInt p = (fromIntegral p) :: Int

-- Very Important!
-- The order of this list is relevant!
-- If you change the order of this list you'll break existing
-- programs making use of the hack mode. DO NOT CHANGE THE ORDER
-- OF THIS LIST! JUST DO NOT!
-- HackMode will not make it into an official version. Screw the order!
builtins = [
  ("j", builtinSwap),
  ("J", builtinDup),
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
  ("ww", builtinWithWords),
  ("WW", builtinWithWordsPretty),
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
  ("sp", builtinSpecialInputPretty),
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
  ("SC", builtinSortByComparing2),
  ("?+", builtinCoerceAdd),
  ("?-", builtinCoerceSub),
  ("?/", builtinCoerceDiv),
  ("?*", builtinCoerceMul),
  ("?^", builtinCoercePow),
  ("?s", builtinCoerceSqrt),
  ("?!", builtinCoerceFactorial),
  ("?i", builtinCoerceInc),
  ("?d", builtinCoerceDec),
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
  ("gr", builtinGrep),
  ("pi", builtinPi),
  ("ee", builtinE),
  ("pm", builtinPlusMinus),
  ("ch", builtinChoose),
  ("f~", builtinSimpleFormat),
  ("rm", builtinRangeModulo),
  ("nc", builtinNormalDCumulative),
  ("nd", builtinNormalDDensity),
  ("Bc", builtinBinomialDCumulative),
  ("Bp", builtinBinomialDProbability),
  ("pc", builtinPoissonDCumulative),
  ("pp", builtinPoissonDProbability),
  ("gc", builtinGeometricDCumulative),
  ("gp", builtinGeometricDProbability),
  ("hc", builtinHypergeometricDCumulative),
  ("hp", builtinHypergeometricDProbability),
  ("cc", builtinChiSquaredDCumulative),
  ("cd", builtinChiSquaredDDensity),
  ("ec", builtinExponentialDCumulative),
  ("ed", builtinExponentialDDensity),
  ("Sc", builtinStudentTDCumulative),
  ("Sd", builtinStudentTDDensity),
  ("uc", builtinUniformDCumulative),
  ("ud", builtinUniformDDensity),
  ("Sq", builtinStudentTDQuantile),
  ("uq", builtinUniformDQuantile),
  ("eq", builtinExponentialDQuantile),
  ("nq", builtinNormalDQuantile),
  ("cq", builtinChiSquaredDQuantile),
  ("f:", builtinFrequencyList),
  ("F:", builtinFrequencyListPercentage),
  ("u[", builtinUnzip),
  ("U[", builtinUngroup),
  ("vr", builtinVariance),
  ("SD", builtinStandardDeviation),
  ("x/", builtinXSwap),
  ("ct", builtinChiSquaredTest),
  ("nr", builtinNCr),
  ("zi", builtinZipIndices),
  ("al", builtinAll),
  ("ay", builtinAny),
  ("ad", builtinAllDigit),
  ("an", builtinAllAlphaNum),
  ("aa", builtinAllAlpha),
  ("w[", builtinFilterWords),
  ("W[", builtinFilterLines),
  ("so", builtinSorted),
  ("SO", builtinSortedReverse),
  ("ic", builtinIntercalate),
  ("=s", builtinSortEqual),
  ("pt", builtinPartition),
  ("es", builtinEmptyBlockToStr),
  ("s=", builtinStrEqIgnoreCase),
  ("gw", builtinGroupWithLength),
  ("gl", builtinGroupLength),
  ("gn", builtinGroupNub),
  ("mo", builtinMultiplesOf),
  (">=", builtinGeq),
  ("<=", builtinLeq),
  ("mm", builtinMmult),
  ("ss", builtinStrStr),
  ("en", builtinEveryNth),
  ("pe", builtinParseEval),
  ("sl", builtinSelectLines),
  ("sw", builtinSelectWords),
  ("di", builtinDeleteIndices),
  ("tl", builtinTrimLines),
  ("td", builtinToDouble),
  ("ti", builtinToInt),
  ("su", builtinSubstrings),
  ("#s", builtinPushStack),
  ("#S", builtinPopStack),
  ("#r", builtinRotateStackLeft),
  ("#R", builtinRotateStackRight),
  ("cl", builtinCeiling),
  ("fo", builtinFloor),
  ("z?", builtinZero),
  ("nz", builtinNotZero),
  ("dg", builtinDigits),
  ("ug", builtinUnDigits),
  ("Pp", builtinPushToState),
  ("PP", builtinPopFromState),
  ("pP", builtinPeekFromState),
  ("p/", builtinSwapOnState),
  ("GO", builtinGenerateListO),
  ("GZ", builtinGenerateListZ),
  ("l2", builtinLogBase2),
  ("l0", builtinLogBase10),
  ("p\\", builtinSwapStacks),
  ("CN", builtinCount),
  ("MV", builtinMove),
  ("C!", builtinContinuationMany),
  ("#<", builtinReverseStack), 
  ("!C", builtinContinuationManyReverse),
  ("TW", builtinTakeWhileSwapped),
  ("DW", builtinDropWhileSwapped),
  ("IC", builtinIntercalateSwapped),
  ("[]", builtinIntersperseSwapped),
  ("CW", builtinChooseWords),
  ("MP", builtinMapPush),
  ("CL", builtinCollectStack),
  ("Cl", builtinCollectStackReverse),
  ("U_", builtinUnique),
  ("sm", builtinSame),
  ("fu", builtinFilterUnlines),
  ("ck", builtinCheck), 
  ("it", builtinIt),
  ("th", builtinThat),
  ("bs", builtinBoxSP),
  ("BS", builtinBoxSP2),
  ("cn", builtinCond),
  ("m&", builtinMkAnd),
  ("m|", builtinMkOr),
  ("m$", builtinMkXor),
  ("M-", builtinCoolMap),
  ("ap", builtinApply),
  ("b2", builtinConvertBase2),
  ("b6", builtinConvertBase16),
  ("b0", builtinConvertBase10),
  ("P_", builtinPopFromState2),
  ("lp", builtinLeftPad),
  ("rp", builtinRightPad),
  ("Q", builtinPretty),
  ("dv", builtinDivides),
  ("rs", builtinRunStack),
  ("RS", builtinRunStack2),
  ("pa", builtinPartial),
  ("iT", builtinInits),
  ("iS", builtinTails),
  ("iR", builtinRotations),
  ("sv", builtinSetVar),
  ("gv", builtinGetVar),
  ("ng", builtinNegate),
  ("fp", builtinFlipBits),
  ("gb", builtinGroupBy),
  ("gB", builtinGroupBy2),
  
  
  ("?_", builtinBuiltins),
  ("?n", builtinBuiltinNth),
  ("??", builtinVersion)
 ]

lookupBuiltin b = fromMaybe (pushToStack (BlsqError ("Unknown command: (" ++ b ++ ")!"))) $ lookup b builtins

putResult = putStack

builtinFlipBits = do
  st <- getStack
  case st of
    (BlsqInt a : xs) -> do
      putResult $ (BlsqInt (complement a)) : xs
    _ -> builtinToInt >> builtinFlipBits

builtinNegate = do
  pushToStack $ BlsqInt (-1)
  builtinCoerceMul

builtinGetVar = do
  st <- getStack
  case st of
    (a : xs) -> do
      putStack xs
      v <- getVar a
      pushToStack v
    _ -> putResult $ BlsqError "Burlesque (gv): Stack empty!" : st
    
builtinSetVar = do
  st <- getStack
  case st of
    (a : v : xs) -> do
      putStack xs
      setVar a v
    _ -> putResult $ BlsqError "Burlesque (sv): Stack almost empty!" : st

-- | ?n
builtinBuiltinNth = do
  (BlsqInt n) <- popFromStack
  pushToStack . BlsqIdent $ fst (builtins !! (toInt n))

-- | ?_
builtinBuiltins = do
  pushToStack . BlsqStr $ "I have " ++ (show $ length builtins) ++ " non-special builtins!"
  
-- | iT
builtinInits = do
  st <- getStack
  case st of
    (BlsqBlock xs : ss) -> do
      putResult $ (BlsqBlock (map BlsqBlock (inits xs))) : ss
    (BlsqStr xs : ss) -> do
      putResult $ (BlsqBlock (map BlsqStr (inits xs))) : ss
    _ -> builtinExplode >> builtinInits
    
-- | iS
builtinTails = do
  st <- getStack
  case st of
    (BlsqBlock xs : ss) -> do
      putResult $ (BlsqBlock (map BlsqBlock (tails xs))) : ss
    (BlsqStr xs : ss) -> do
      putResult $ (BlsqBlock (map BlsqStr (tails xs))) : ss
    _ -> builtinExplode >> builtinTails
    
-- | iR
builtinRotations = do
  st <- getStack
  case st of
    (BlsqBlock xs : ss) -> do
      putStack []
      pushToStack $ BlsqBlock xs
      pushToStack $ BlsqBlock [BlsqIdent "rt"]
      pushToStack $ BlsqInt (fromIntegral $ length xs)
      builtinContinuationMany
      st' <- getStack
      putStack $ (BlsqBlock (init st')) : ss
    (BlsqStr str : ss) -> do
      builtinExplode
      builtinRotations
      pushToStack $ BlsqStr ")\\["
      builtinParseEval

-- | dv
builtinDivides = do
  builtinMod
  builtinNot
  
-- | lp
builtinLeftPad = do
  pushToStack $ BlsqStr "x/Shx/\\/x/x/\\/P["
  builtinParse
  builtinEval
  
-- | rp
builtinRightPad = do
  pushToStack $ BlsqStr "x/Shx/\\/x/x/\\/[P"
  builtinParse
  builtinEval

-- | b2
builtinConvertBase2 = do
  pushToStack $ BlsqInt 2
  builtinConvertBase
  
-- | b6
builtinConvertBase16 = do
  pushToStack $ BlsqInt 16
  builtinConvertBase

-- | b0
builtinConvertBase10 = do
  pushToStack $ BlsqInt 10
  builtinConvertBase
  
-- | ap
builtinApply = do
  st <- getStack
  case st of 
    (BlsqBlock f : BlsqInt idx : BlsqBlock xs : ss) -> do
      putStack ss
      pushToStack $ BlsqBlock xs
      pushToStack $ BlsqInt idx
      builtinBlockAccess
      pushToStack $ BlsqBlock f
      builtinEval
      nelem <- popFromStack
      pushToStack $ BlsqBlock xs
      pushToStack nelem
      pushToStack $ BlsqInt idx
      builtinSetAt
    (BlsqInt idx : BlsqBlock f : BlsqBlock xs : ss) -> do
      builtinSwap
      builtinApply
    _ -> pushToStack $ BlsqError "Burlesque (ap): Invalid arguments!"
  
-- | M-
builtinCoolMap = do
  pushToStack $ BlsqStr "\\/bxcy\\/z[{p^+]e!}m["
  builtinParse
  builtinEval
  
-- | m&
builtinMkAnd = do
  st <- getStack
  case st of
    (BlsqBlock a : BlsqBlock b : xs) -> do
      putStack xs
      builtinDup
      pushToStack $ BlsqBlock a
      builtinEval
      builtinSwap
      pushToStack $ BlsqBlock b
      builtinEval
      builtinAnd
    _ -> pushToStack $ BlsqError "Burlesque (m&): Invalid arguments!"
    
-- | m|
builtinMkOr = do
  st <- getStack
  case st of
    (BlsqBlock a : BlsqBlock b : xs) -> do
      putStack xs
      builtinDup
      pushToStack $ BlsqBlock a
      builtinEval
      builtinSwap
      pushToStack $ BlsqBlock b
      builtinEval
      builtinOr
    _ -> pushToStack $ BlsqError "Burlesque (m&): Invalid arguments!"
    
-- | m$
builtinMkXor = do
  st <- getStack
  case st of
    (BlsqBlock a : BlsqBlock b : xs) -> do
      putStack xs
      builtinDup
      pushToStack $ BlsqBlock a
      builtinEval
      builtinSwap
      pushToStack $ BlsqBlock b
      builtinEval
      builtinXor
    _ -> pushToStack $ BlsqError "Burlesque (m&): Invalid arguments!"
  
-- | cn
builtinCond = do
  st <- getStack
  case st of
    (BlsqBlock xs : ss) -> do
      putStack ss
      cond' xs ss
    _ -> pushToStack $ BlsqError "Burlesque (cn): Invalid arguments!"
  where cond' (x:xs) os = do
           pushToStack x
           builtinEval
           st' <- getStack
           case st' of
             (BlsqInt 0 : ss) -> putStack os >> cond' (tail xs) os
             _ -> putStack $ (head xs) : (tail os)
        cond' [] os = putStack $ os
  
-- | BS
builtinBoxSP2 = do
  builtinBox
  builtinSpecialInput
  builtinPretty
  
-- | bs
builtinBoxSP = do
  builtinBox
  builtinSpecialInput
  
-- | th
builtinThat = do
  st <- getStack
  putStack [last st]
  
-- | it
builtinIt = do
  t <- popFromStack
  putStack [t]

-- | ck
builtinCheck = do
  builtinNot
  builtinNot

-- | fu
builtinFilterUnlines = do
  builtinFilter
  builtinUnlines

-- | sm
builtinSame = do
  builtinDup
  builtinHead
  t <- popFromStack
  pushToStack $ BlsqBlock [t, BlsqIdent "=="]
  builtinAll

-- | U_
builtinUnique = do
  builtinDup
  builtinNub
  builtinEqual

-- | Cl
builtinCollectStackReverse = do
  builtinCollectStack
  builtinReverse

-- | CL
builtinCollectStack = do
  st <- getStack
  putStack $ [BlsqBlock st]

-- | MP
builtinMapPush = do
  builtinMap
  builtinPushMany

-- | CW
builtinChooseWords = do
  builtinWords
  builtinChoose

-- | TW
builtinTakeWhileSwapped = do
  builtinSwap
  builtinTakeWhile

-- | DW
builtinDropWhileSwapped = do
  builtinSwap
  builtinDropWhile

-- | IC
builtinIntercalateSwapped = do
  builtinSwap
  builtinIntercalate

-- | []
builtinIntersperseSwapped = do
  builtinSwap
  builtinIntersperse

-- | !C
builtinContinuationManyReverse :: BlsqState
builtinContinuationManyReverse = do
  builtinContinuationMany
  builtinReverseStack

-- | #<
builtinReverseStack :: BlsqState
builtinReverseStack = do
  st <- getStack
  putStack (reverse st)

-- | C!
builtinContinuationMany :: BlsqState
builtinContinuationMany = do
 st <- getStack
 case st of
   (BlsqInt n : BlsqBlock cont : xs) -> do
     let ls = (intersperse (BlsqIdent "c!") $ replicate (toInt n) (BlsqBlock cont)) ++ [BlsqIdent "c!"]
     putStack xs
     pushToStack (BlsqBlock ls)
     builtinEval

-- | MV
builtinMove = do 
  st <- getStack
  case st of
    (BlsqInt n : xs) -> putStack $ (xs !! (toInt n)) : (removeAt (toInt n) xs)
    _ -> pushToStack $ BlsqError "Burlesque (MV): Invalid arguments!"

-- | CN
builtinCount :: BlsqState
builtinCount = do 
  i <- popFromStack
  pushToStack (BlsqBlock [i, BlsqIdent "=="])
  builtinFilterLength
  
-- | l2
builtinLogBase2 :: BlsqState
builtinLogBase2 = do
  pushToStack (BlsqInt 2)
  builtinSwap
  builtinLog2
  
-- | l0
builtinLogBase10 :: BlsqState
builtinLogBase10 = do
  pushToStack (BlsqInt 10)
  builtinSwap
  builtinLog2

-- | GO
builtinGenerateListO :: BlsqState
builtinGenerateListO = do
  s <- popFromStack
  pushToStack (BlsqInt 1)
  builtinSwap
  builtinRange
  pushToStack s
  builtinMap
  
-- | GZ
builtinGenerateListZ :: BlsqState
builtinGenerateListZ = do
  s <- popFromStack
  pushToStack (BlsqInt 0)
  builtinSwap
  builtinRange
  pushToStack s
  builtinMap
  

-- | > .+
builtinAdd :: BlsqState
builtinAdd = do
 st <- getStack
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
    
    ((BlsqInt a):(BlsqStr b):xs) -> (BlsqStr $ genericTake a b) : xs
    ((BlsqInt a):(BlsqBlock b):xs) -> (BlsqBlock $ genericTake a b) : xs
    
    (BlsqInt a : BlsqDouble b : xs) -> (BlsqDouble $ (fromIntegral a) + b) : xs
    (BlsqDouble a : BlsqInt b : xs) -> (BlsqDouble $ a + (fromIntegral b)) : xs
    _ -> (BlsqError $ "Burlesque: (.+) Invalid arguments!") : st

-- | > _+
builtinAddX :: BlsqState
builtinAddX = do
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
    ((BlsqInt b):(BlsqInt a):xs) -> (BlsqInt (a - b)) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a - b)) : xs
    ((BlsqStr b):(BlsqInt a):xs) -> (BlsqStr $ genericDrop a b) : xs
    ((BlsqStr b):(BlsqStr a):xs) -> if b `isSuffixOf` a
                                     then (BlsqStr $ genericTake (genericLength a - length b) a) : xs
                                     else (BlsqStr a) : xs
    ((BlsqBlock b):(BlsqInt a):xs) -> (BlsqBlock $ genericDrop a b) : xs
    
    ((BlsqInt a):(BlsqStr b):xs) -> (BlsqStr $ genericDrop a b) : xs
    ((BlsqInt a):(BlsqBlock b):xs) -> (BlsqBlock $ genericDrop a b) : xs
    
    (BlsqInt a : BlsqDouble b : xs) -> (BlsqDouble $ b - (fromIntegral a)) : xs
    (BlsqDouble a : BlsqInt b : xs) -> (BlsqDouble $ (fromIntegral b) - a) : xs
    ((BlsqBlock b):(BlsqBlock a):xs) -> if b `isSuffixOf` a
                                     then (BlsqBlock $ genericTake (genericLength a - length b) a) : xs
                                     else (BlsqBlock a) : xs
    _ -> (BlsqError "Burlesque: (.-) Invalid arguments!") : st

-- | > .*
builtinMul :: BlsqState
builtinMul = do
 st <- getStack
 putResult $
  case st of
    (BlsqInt b : BlsqInt a : xs) -> BlsqInt (a * b) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a * b)) : xs
    (BlsqInt b : BlsqStr a : xs) -> BlsqBlock (genericReplicate b (BlsqStr a)) : xs
    (BlsqInt b : BlsqChar a : xs) -> BlsqStr (genericReplicate b a) : xs
    (BlsqInt b : BlsqBlock a : xs) -> BlsqBlock (genericReplicate b (BlsqBlock a)) : xs
    (BlsqStr a : BlsqStr b : xs) -> BlsqStr (reverse $ a++b) : xs
    
    (BlsqInt a : BlsqDouble b : xs) -> (BlsqDouble $ (fromIntegral a) * b) : xs
    (BlsqDouble a : BlsqInt b : xs) -> (BlsqDouble $ a * (fromIntegral b)) : xs
    _ -> (BlsqError "Burlesque: (.*) Invalid arguments!") : st

-- | > ./
builtinDiv :: BlsqState
builtinDiv = do
 st <- getStack
 putResult $
  case st of
    (BlsqInt b : BlsqInt a : xs) -> BlsqInt (a `div` b) : xs
    ((BlsqDouble b):(BlsqDouble a):xs) -> (BlsqDouble (a / b)) : xs
    (BlsqStr a : BlsqStr b : xs) -> case a `isPrefixOf` b of
                                 True -> BlsqStr (drop (length a) b) : xs
                                 False -> BlsqStr b : xs
                                 
    (BlsqInt a : BlsqDouble b : xs) -> (BlsqDouble $ b / (fromIntegral a)) : xs
    (BlsqDouble a : BlsqInt b : xs) -> (BlsqDouble $ (fromIntegral b) / a) : xs
    _ -> (BlsqError "Burlesque: (./) Invalid arguments!") : st

-- | .%
builtinMod :: BlsqState
builtinMod = do
  st <- getStack
  case st of
    (BlsqInt b : BlsqInt a : xs) -> putResult $ BlsqInt (a `mod`b) : xs
    (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                         builtinBoxCycle
                                         builtinSwap
                                         builtinMod
    (BlsqInt a : BlsqBlock b : xs) -> do builtinBoxCycle
                                         builtinMod
    (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent ".%" ])
                                           builtinZipWithPush
    _ -> putResult $ (BlsqError "Burlesque: (.%) Invalid arguments!") : st

-- | > +.
builtinIncrement :: BlsqState
builtinIncrement = do
 st <- getStack
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
 st <- getStack
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
 st <- getStack
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
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (BlsqStr a) : xs -> (BlsqBlock . map BlsqStr . lines $ a) : xs
   (BlsqInt a) : xs -> (BlsqInt . genericLength . show $ a) : xs
   _ -> (BlsqError "Burlesque: (ln) Invalid arguments!") : st

-- | > un
builtinUnlines :: BlsqState
builtinUnlines = do
 st <- getStack
 case st of
  (BlsqBlock [] : xs) -> putResult $ BlsqStr "" : xs
  _ -> do pushToStack (BlsqStr "\n")
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
 st <- getStack
 putResult $
  case st of
   (BlsqBlock b : a : xs) -> (BlsqBlock $ intersperse a b) : xs
   (BlsqStr b : BlsqChar a : xs) -> (BlsqStr $ intersperse a b) : xs
   _ -> (BlsqError "Burlesque: ([[) Invalid arguments!") : st

-- | > ri
builtinReadInt :: BlsqState
builtinReadInt = do
 st <- getStack
 case st of
   (BlsqStr a) : xs -> putResult $ (BlsqInt . read $ a) : xs
   (BlsqInt a) : xs -> putResult $ (BlsqInt a) : xs
   (BlsqDouble a : xs) -> builtinAverage
   (BlsqChar a) : xs -> putResult $ BlsqInt (if isAlphaNum a then 1 else 0) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "ri" ]) >> builtinMap
   _ -> putResult $ (BlsqError "Burlesque: (ri) Invalid arguments!") : st

-- | > rd
builtinReadDouble :: BlsqState
builtinReadDouble = do
 st <- getStack
 case st of
   (BlsqStr a) : xs -> putResult $ (BlsqDouble . read $ a) : xs
   (BlsqInt a : xs) -> builtinProduct
   (BlsqDouble a) : xs -> putResult $ (BlsqDouble a) : xs
   (BlsqChar a) : xs -> putResult $ BlsqInt (if isAlpha a then 1 else 0) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "rd" ]) >> builtinMap
   _ -> putResult $ (BlsqError "Burlesque: (rd) Invalid arguments!") : st

-- | > ra
builtinReadArray :: BlsqState
builtinReadArray = do
 st <- getStack
 case st of
   (BlsqStr a) : xs -> putResult $ (runParserWithString' parseData a) : xs
   (BlsqChar a) : xs -> putResult $ BlsqInt (if isSpace a then 1 else 0) :xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "ra" ]) >> builtinMap
   _ -> putResult $ (BlsqError "Burlesque: (ra) Invalid arguments!") : st

-- | > ps
builtinParse :: BlsqState
builtinParse = do
 st <- getStack
 case st of
   (BlsqStr a) : xs -> putResult $ (BlsqBlock (runParserWithString parseBlsq a)) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "ps" ]) >> builtinMap
   _ -> putResult $ (BlsqError "Burlesque: (ps) Invalid arguments!") : st

-- | > up
builtinUnparse :: BlsqState
builtinUnparse = do
 st <- getStack
 putResult $
  case st of 
   (a : xs) -> BlsqStr (toDisplay a) : xs
   _ -> (BlsqError "Burlesque: (up) Invalid arguments!") : st

-- | > ++
builtinSum :: BlsqState
builtinSum = do
 st <- getStack
 case st of
   (BlsqBlock [] : xs) -> do putResult $ BlsqInt 0 : xs
   (BlsqBlock a) : xs -> do pushToStack (BlsqBlock [BlsqIdent ".+"])
                            builtinReduce
   (BlsqInt b : BlsqInt a : xs) -> putResult $ (BlsqInt . read $ (show (abs a)) ++ (show (abs b))) : xs
   _ -> putResult $ (BlsqError "Burlesque: (++) Invalid arguments!") : st

builtinProduct :: BlsqState
builtinProduct = do
 st <- getStack
 case st of
   (BlsqBlock [] : xs) -> do putResult $ BlsqInt 1 : xs
   (BlsqBlock a) : xs -> do pushToStack (BlsqBlock [BlsqIdent ".*"] )
                            builtinReduce
   (BlsqInt _ : xs) -> do builtinPrettyFromFormat
                          builtinReadDouble
   (BlsqDouble a : xs) -> do putResult $ (BlsqInt . ceiling $ a) : xs
   _ -> putResult $ (BlsqError "Burlesque: (pd) Invalid arguments!"): st

builtinProductMany :: BlsqState
builtinProductMany = do pushToStack(BlsqBlock [BlsqIdent "pd"])
                        builtinMap

builtinAverage :: BlsqState
builtinAverage = do
 st <- getStack
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
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a) : xs -> (BlsqBlock (init a)) : xs
   (BlsqStr a) : xs -> BlsqStr (init a) : xs
   (BlsqInt a) : xs -> BlsqInt (read . init . show . abs $ a) : xs
   _ -> (BlsqError "Burlesque: (~]) Invalid arguments!") : st

-- | > [-
builtinTail :: BlsqState
builtinTail = do
 st <- getStack
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
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (b : BlsqBlock a : xs) -> BlsqBlock (a ++ [b]) : xs
   (BlsqChar b : BlsqStr a : xs) -> BlsqStr (a ++ [b]) : xs
   (BlsqInt b : BlsqInt a : xs) -> (BlsqInt . read $ (show . abs $ a) ++ (show . abs $ b)) : xs
   _ -> (BlsqError "Burlesque: ([+) Invalid arguments!") : st

-- | > +]
builtinPrepend :: BlsqState
builtinPrepend = do
 st <- getStack
 putResult $
  case st of
   (b : BlsqBlock a : xs) -> BlsqBlock (b : a) : xs
   (BlsqChar b : BlsqStr a : xs) -> BlsqStr (b : a) : xs
   (BlsqInt b : BlsqInt a : xs) -> (BlsqInt . read $ (show . abs $ b) ++ (show . abs $ a)) : xs
   _ -> (BlsqError "Burlesque: (+]) Invalid arguments!") : st

-- | > \[
builtinConcat :: BlsqState
builtinConcat = do
 st <- getStack
 case st of
  -- Special case for empty block
  (BlsqBlock [] : xs) -> do putResult $ BlsqBlock [] : xs
  -- Special case for single char blocks
  (BlsqBlock [BlsqChar a] : xs) -> do putResult $ BlsqStr [a] : xs
  (BlsqBlock a) : xs -> do
     pushToStack ((BlsqBlock $ [BlsqIdent "_+"]))
     builtinReduce
     st' <- getStack
     case st' of 
      (BlsqStr _ : xs) -> return ()
      (BlsqBlock _ : xs) -> return ()
      (a : xs) -> putResult $ BlsqBlock [a] : xs
  _ -> putResult $ (BlsqError "Burlesque: (\\[) Invalid arguments!") : st

-- | > m[
builtinMap :: BlsqState
builtinMap = do
 st <- getStack
 case st of
   (BlsqBlock v : BlsqStr f : xs) -> do
      builtinSwap
      builtinExplode
      builtinSwap
      builtinMap
      builtinConcat
   (BlsqBlock a : BlsqBlock b : xs) -> do 
      (s , g, v) <- get
      let (mst, ng, nv) = map' a b g v
      put (s, ng, nv)
      putResult $ (BlsqBlock $ mst) : xs
   _ -> putResult $ BlsqError "Burlesque: (m[) Invalid arguments!" : st
 where map' _ [] g v = ([], g, v)
       map' f (x:xs) g v = 
         let (st, ng, nv) = runStack'' f [x] g v
             (st', ng', nv') = map' f xs ng nv
         in (st++st', ng', nv')    

builtinPartial :: BlsqState
builtinPartial = do
  st <- getStack
  case st of
    (BlsqBlock f : BlsqBlock xs : ss) -> do
      putStack ss
      (s, g, v) <- get
      let (nst, ng, nv) = partial' f xs g v
      put (s, ng, nv)
      putResult $ (BlsqBlock nst) : ss
      builtinReverse
    _ -> putResult $ BlsqError "Burlesque: (pa) Invalid arguments!" : st
 where partial' _ [] g v = ([], g, v)
       partial' f xs g v = 
         let (st, ng, nv) = runStack'' f [BlsqBlock xs] g v
             (st', ng', nv') = partial' f (init xs) ng nv
         in (st++st', ng', nv')             
         
 {-
       map' _ [] = return []
       map' f (x:xs) = do
        st' <- runStack f [x]
        st'' <- map' f xs
        return $ st' ++ st''
       --(runStack f [x]) ++ (map' f xs)-}

-- | > f[
builtinFilter :: BlsqState
builtinFilter = do
 st <- getStack
 case st of
  (BlsqBlock v : BlsqStr f : xs) -> do
      builtinSwap
      builtinExplode
      builtinSwap
      builtinFilter
      st' <- getStack
      case st' of
        (BlsqBlock a : xs) -> case null a of
                                True -> putResult $ BlsqStr "" : xs
                                False -> do builtinConcat
                                            boxString
        _ -> return ()
  (BlsqBlock f : BlsqBlock v : xs) -> do
    (s, g, vv) <- get
    let (ff, ng, nvv) = filter' f v g vv
    put (s, ng, nvv)
    putResult $ (BlsqBlock $ ff) : xs
  _ -> putResult $ BlsqError "Burlesque: (f[) Invalid arguments!" : st
 where filter' _ [] g vv = ([], g, vv)
       filter' f (x:xs) g vv = 
         let (st, ng, nvv) = runStack'' f [x] g vv in
         case st of
           (BlsqInt 0):ys -> filter' f xs ng nvv
           _ -> let (ys, ng', nvv') = filter' f xs ng nvv in
                (x : ys, ng', nvv')
       {-
       filter' _ [] = return []
       filter' f (x:xs) = do
                           rsf <- runStack f [x]
                           case rsf of
                            (BlsqInt 0):ys -> filter' f xs
                            _ -> do 
                               xx <- filter' f xs
                               return $ x : xx-}
       boxString = do
         st <- getStack
         case st of
          (BlsqChar a : xs) -> putResult $ BlsqStr [a] : xs
          _ -> return ()

-- | > r[
builtinReduce :: BlsqState
builtinReduce = do
 st <- getStack
 case st of
   (BlsqBlock f : BlsqBlock ls : xs) -> do
     rr <- (reduce' f ls)
     putResult $ rr : xs
   _ -> putResult $ BlsqError "Burlesque: (r[) Invalid arguments!" : st
 where reduce' f [] = return $ BlsqError "Burlesque: (r[) Empty list!"
       reduce' f (x:xs) = reduce'' f x xs
       reduce'' f z [] = return $z
       reduce'' f z (x:xs) = do
                             rsf <- runStack f [x,z]
                             case rsf of
                              (a : ys) -> reduce'' f a xs
                              _ -> return $ BlsqError "Burlesque: (r[) Stack size error!"

-- | > wl
builtinWithLines :: BlsqState
builtinWithLines = do
 builtinSwap
 builtinLines
 builtinSwap
 builtinMap
 builtinUnlines
 
-- | > ww
builtinWithWords :: BlsqState
builtinWithWords = do
 builtinSwap
 builtinWords2
 builtinSwap
 builtinMap
 builtinWords
 
-- | > WW
builtinWithWordsPretty :: BlsqState
builtinWithWordsPretty = do
 builtinWithWords
 builtinPretty

-- | > WL
builtinWithLinesPretty :: BlsqState
builtinWithLinesPretty = do
 builtinWithLines
 builtinPretty

-- | > wL
builtinWithLinesParsePretty :: BlsqState
builtinWithLinesParsePretty = do
 pushToStack (BlsqIdent "ps")
 builtinPrepend
 builtinWithLinesPretty
 

-- | > \/
builtinSwap :: BlsqState
builtinSwap = do
 st <- getStack
 putResult $
  case st of
   (a : b : xs) -> b : a : xs
   _ -> BlsqError "Burlesque: (\\/) Stack size error!" : st

-- | > ^^
builtinDup :: BlsqState
builtinDup = do
 st <- getStack
 putResult $
  case st of
   (a : xs) -> (a : a : xs)
   _ -> BlsqError "Burlesque: (^^) Stack size error!" : st

-- | > vv
builtinPop :: BlsqState
builtinPop = do
 st <- getStack
 putResult $
  case st of
   (a : xs) -> xs
   _ -> BlsqError "Burlesque: (vv) Stack size error!" : st

-- | > if
builtinIff :: BlsqState
builtinIff = do
 st <- getStack
 case st of
   (BlsqInt 0 : BlsqBlock b : xs) -> putResult $ xs
   (BlsqInt _ : BlsqBlock b : xs) -> do
     rr <- runStack b xs
     putResult rr
   (BlsqBlock b : BlsqInt 0 : xs) -> putResult $ xs
   (BlsqBlock b : BlsqInt _ : xs) -> do
     rr <- runStack b xs
     putResult rr
   _ -> putResult $ BlsqError "Burlesque: (if) Invalid arguments!" : st

-- | > ie
builtinIfElse:: BlsqState
builtinIfElse = do
 st <- getStack
 case st of
   (BlsqInt 0 : BlsqBlock b : BlsqBlock a : xs) -> do
     rr <- runStack b xs
     putResult rr
   (BlsqInt _ : BlsqBlock b : BlsqBlock a : xs) -> do
     rr <- runStack a xs
     putResult rr
   _ -> putResult $ BlsqError "Burlesque: (ie Invalid arguments!" : st

-- | > e!
builtinEval :: BlsqState
builtinEval = do
 st <- getStack
 case st of
   (BlsqBlock b : xs) -> do
     rr <- runStack b xs
     putResult rr
   _ -> putResult $ BlsqError "Burlesque: (e!) Invalid arguments!" : st

-- | > E!
builtinEvalMany :: BlsqState
builtinEvalMany = do
 builtinMul
 builtinConcat
 builtinEval

-- | > c!
builtinContinuation :: BlsqState
builtinContinuation = do
 st <- getStack
 case st of
   (BlsqBlock b : xs) -> do
                           rs <- runStack b xs
                           case rs of
                            (a:ys) -> putResult $ a : xs
                            _ -> putResult $ st
   _ -> putResult $ BlsqError "Burlesque: (c!) Invalid arguments!" : st

-- | > w!
builtinWhile :: BlsqState
builtinWhile = do
 st <- getStack
 case st of
   (BlsqBlock b : BlsqBlock a : xs) -> while' a b xs
   (BlsqBlock a : xs) -> while' a [] xs
   _ -> putResult $ BlsqError "Burlesque: (w!) Invalid arguments!" : st
 where while' f g xs = do
                       rs <- runStack g xs
                       case rs of
                        (BlsqInt 0 : ys) -> putResult $ xs
                        (BlsqInt a : ys) -> do
                          rs <- runStack f xs
                          while' f g $ rs
                        (_ : ys) -> putResult $ BlsqError "Burlesque: (w!) Invalid!" : ys
                        _ -> putResult $ BlsqError "Burlesque: (w!) Stack size error!" : xs

-- | > (.>)
builtinGreater :: BlsqState
builtinGreater = do
 st <- getStack
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a > b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (.>) Invalid arguments!" : st

-- | > (.<)
builtinSmaller :: BlsqState
builtinSmaller = do
 st <- getStack
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a < b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (.<) Invalid arguments!" : st
   
-- | > (>=)
builtinGeq :: BlsqState
builtinGeq = do
 st <- getStack
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a >= b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (>=) Invalid arguments!" : st

-- | > (<=)
builtinLeq :: BlsqState
builtinLeq = do
 st <- getStack
 putResult $
  case st of
   (b : a : xs) -> (BlsqInt $ if a <= b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (<=) Invalid arguments!" : st

-- | > (>.)
builtinMax :: BlsqState
builtinMax = do
 st <- getStack
 putResult $
  case st of
   (b : a : xs) -> (max a b) : xs
   _ -> BlsqError "Burlesque: (>.) Stack size error!" : st

-- | > (<.)
builtinMin :: BlsqState
builtinMin = do
 st <- getStack
 putResult $
  case st of
   (b : a : xs) -> (min a b) : xs
   _ -> BlsqError "Burlesque: (<.) Stack size error!" : st

-- | > (>])
builtinMaximum :: BlsqState
builtinMaximum = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a : xs) -> (maximum a) : xs
   (BlsqStr a : xs) -> BlsqChar (maximum a) : xs
   (BlsqInt a : xs) -> BlsqInt (read . return . maximum . show $ a) : xs
   _ -> BlsqError "Burlesque: (>]) Invalid arguments!" : st

-- | > (<])
builtinMinimum :: BlsqState
builtinMinimum = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a : xs) -> (minimum a) : xs
   (BlsqStr a : xs) -> BlsqChar (minimum a) : xs
   (BlsqInt a : xs) -> BlsqInt (read . return . minimum . show $ a) : xs
   _ -> BlsqError "Burlesque: (<]) Invalid arguments!" : st


-- | > (==)
builtinEqual :: BlsqState
builtinEqual = do
 st <- getStack
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
 st <- getStack
 case st of
   (BlsqInt b : BlsqDouble a : xs) -> putResult $ (BlsqDouble $ round' a b) : xs
   (BlsqInt b : BlsqBlock a : xs) -> do builtinPop
                                        pushToStack (BlsqBlock [ BlsqInt b , BlsqIdent "r_" ])
                                        builtinMap
   _ -> putResult $ BlsqError "Burlesque: (r_) Invalid arguments!" : st
 where round' n s = let factor = fromIntegral (10^s) in fromIntegral (round (n * factor)) / factor

-- | R_
builtinRound2 :: BlsqState
builtinRound2 = do
 pushToStack (BlsqInt 0)
 builtinRound
 builtinProduct

-- | > (XX)
builtinExplode :: BlsqState
builtinExplode = do
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (a : xs) -> (BlsqPretty a BlsqFormatNormal) : xs
   _ -> BlsqError "Burlesque: (sh) Invalid arguments!" : st

-- | > ~[
builtinContains :: BlsqState
builtinContains = do
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> BlsqInt (if a `isInfixOf` b then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (~~) Invalid arguments!" : st

-- | > ~!
builtinPrefixOf :: BlsqState
builtinPrefixOf = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> BlsqInt (if a `isPrefixOf` b then 1 else 0) : xs
   (BlsqStr a : BlsqStr b : xs) -> BlsqInt (if a `isPrefixOf` b then 1 else 0) : xs
   (BlsqInt a : BlsqInt b : xs) -> BlsqInt (if (show . abs $ a) `isPrefixOf` (show . abs $ b) then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (~!) Invalid arguments!" : st

-- | > !~
builtinSuffixOf :: BlsqState
builtinSuffixOf = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> BlsqInt (if a `isSuffixOf` b then 1 else 0) : xs
   (BlsqStr a : BlsqStr b : xs) -> BlsqInt (if a `isSuffixOf` b then 1 else 0) : xs
   (BlsqInt a : BlsqInt b : xs) -> BlsqInt (if (show . abs $ a) `isSuffixOf` (show . abs $ b) then 1 else 0) : xs
   _ -> BlsqError "Burlesque: (!~) Invalid arguments!" : st

-- | > r~
builtinReplace :: BlsqState
builtinReplace = do
 st <- getStack
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
 st <- getStack
 case st of
  (BlsqBlock ls : xs) -> do
     builtinPop
     mapM (\v -> pushToStack (v)) ls
 return ()

-- | > p^
builtinPushManyReverse :: BlsqState
builtinPushManyReverse = do
 st <- getStack
 case st of
  (BlsqBlock ls : xs) -> do 
     builtinPop
     mapM (\v -> pushToStack (v)) (reverse ls)
 return ()

-- | > =[
builtinGroup :: BlsqState
builtinGroup = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock ls : xs) -> (BlsqBlock $ map (BlsqBlock) (group ls)) : xs
   (BlsqStr ls : xs) -> (BlsqBlock $ map (BlsqStr) (group ls)) : xs
   _ -> BlsqError "Burlesque: (=[) Invalid arguments!" : st

-- | > FF
builtinFormat :: BlsqState
builtinFormat = do
 st <- getStack
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
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (BlsqStr regex : BlsqStr str : xs) -> (case matchRegex (mkRegex regex) str of
                                            Just q -> BlsqInt 1
                                            _ -> BlsqInt 0) : xs
   _ -> BlsqError "Burlesque: (~=) Invalid arguments!" : st

-- | > =~
builtinMatchesList :: BlsqState
builtinMatchesList = do
 st <- getStack
 putResult $
  case st of
   (BlsqStr regex : BlsqStr str : xs) -> (case matchRegex (mkRegex regex) str of
                                            Just q -> BlsqBlock $ map BlsqStr q
                                            _ -> BlsqBlock []) : xs
   _ -> BlsqError "Burlesque: (~=) Invalid arguments!" : st

-- | > R~
builtinReplaceRegex :: BlsqState
builtinReplaceRegex = do
 st <- getStack
 putResult $
  case st of
   (BlsqStr regex : BlsqStr repl : BlsqStr str : xs) -> 
           BlsqStr (subRegex (mkRegex regex) str repl) : xs
   _ -> BlsqError "Burlesque: (~=) Invalid arguments!" : st

-- | ||
builtinOr :: BlsqState
builtinOr = do
 st <- getStack
 case st of
   (BlsqInt a : BlsqInt b : xs) -> putResult $ (BlsqInt $ a .|. b) : xs
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBoxCycle
                                        builtinSwap
                                        builtinOr
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBoxCycle
                                        builtinOr
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "||" ])
                                          builtinZipWithPush
   _ -> putResult $ BlsqError "Burlesque: (||) Invalid arguments!": st

-- | &&
builtinAnd :: BlsqState
builtinAnd = do
 st <- getStack
 case st of
   (BlsqInt a : BlsqInt b : xs) -> putResult $ (BlsqInt $ a .&. b) : xs
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBoxCycle
                                        builtinSwap
                                        builtinAnd
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBoxCycle
                                        builtinAnd
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "&&" ])
                                          builtinZipWithPush
   _ -> putResult $ BlsqError "Burlesque: (&&) Invalid arguments!": st

-- | $$
builtinXor :: BlsqState
builtinXor = do
 st <- getStack
 case st of
   (BlsqInt a : BlsqInt b : xs) -> putResult $ (BlsqInt $ a `xor` b) : xs
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBoxCycle
                                        builtinSwap
                                        builtinXor
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBoxCycle
                                        builtinXor
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "$$" ])
                                          builtinZipWithPush
   _ -> putResult $ BlsqError "Burlesque: ($$) Invalid arguments!": st

-- | L[
builtinLength :: BlsqState
builtinLength = do
 st <- getStack
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
 st <- getStack
 putResult $ 
  case st of
   (BlsqInt a : xs) -> (BlsqInt $ abs a) : xs
   (BlsqDouble a : xs) -> (BlsqDouble $ abs a) : xs
   _ -> BlsqError "Burlesque: (ab) Invalid arguments!" : st

-- | sn
builtinSignum :: BlsqState
builtinSignum = do
 st <- getStack
 putResult $ 
  case st of
   (BlsqInt a : xs) -> (BlsqInt $ signum a) : xs
   (BlsqDouble a : xs) -> (BlsqDouble $ signum a) : xs
   _ -> BlsqError "Burlesque: (sn) Invalid arguments!" : st

-- | S[
builtinStripLeft :: BlsqState
builtinStripLeft = do
 st <- getStack
 putResult $
  case st of
   (a : BlsqBlock b : xs) -> (BlsqBlock $ dropWhile (==a) b) : xs
   (BlsqChar a : BlsqStr b : xs) -> (BlsqStr $ dropWhile (==a) b) : xs
   (BlsqInt a : xs) -> BlsqInt (a * a) : xs
   _ -> BlsqError "Burlesque: (S[) Invalid arguments!" : st

-- | S[
builtinStripRight :: BlsqState
builtinStripRight = do
 st <- getStack
 putResult $
  case st of
   (a : BlsqBlock b : xs) -> (BlsqBlock .reverse $ dropWhile (==a) (reverse b)) : xs
   (BlsqChar a : BlsqStr b : xs) -> (BlsqStr . reverse $ dropWhile (==a) (reverse b)) : xs
   _ -> BlsqError "Burlesque: ([S) Invalid arguments!" : st

-- | P[
builtinPadLeft :: BlsqState
builtinPadLeft = do
 st <- getStack
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
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a : BlsqBlock b : xs) -> BlsqBlock (map BlsqBlock (splitOn a b)) : xs
   (BlsqStr a : BlsqStr b : xs) -> BlsqBlock (map BlsqStr (splitOn a b)) : xs
   (BlsqInt a : BlsqInt b : xs) -> BlsqBlock (map (BlsqInt . read) (splitOn (show (abs a)) (show (abs b)))) : xs
   _ -> BlsqError "Burlesque: (;;) Invalid arguments!" : st

-- | UN
builtinUnion :: BlsqState
builtinUnion = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (union a b) : xs
   (BlsqStr b : BlsqStr a : xs) -> BlsqStr (union a b) : xs
   (BlsqInt b : BlsqInt a : xs) -> BlsqInt (read $ union (show (abs a)) (show (abs b))) : xs
   _ -> BlsqError "Burlesque: (UN) Invalid arguments!" : st

-- | IN
builtinIntersection :: BlsqState
builtinIntersection = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (intersect a b) : xs
   (BlsqStr b : BlsqStr a : xs) -> BlsqStr (intersect a b) : xs
   (BlsqInt b : BlsqInt a : xs) -> BlsqInt (read $ intersect (show (abs a)) (show (abs b))) : xs
   _ -> BlsqError "Burlesque: (IN) Invalid arguments!" : st

-- | NB
builtinNub :: BlsqState
builtinNub = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a : xs) -> BlsqBlock (nub a) : xs
   (BlsqStr a : xs) -> BlsqStr (nub a) : xs
   (BlsqInt a : xs) -> BlsqInt (read $ nub (show (abs a))) : xs
   _ -> BlsqError "Burlesque: (NB) Invalid arguments!" : st

-- | \\
builtinDiffLs :: BlsqState
builtinDiffLs = do
 st <- getStack
 putResult $
  case st of
   (BlsqBlock b : BlsqBlock a : xs) -> BlsqBlock (a \\ b) : xs
   (BlsqStr b : BlsqStr a : xs) -> BlsqStr (a \\ b) : xs
   (BlsqInt b : BlsqInt a : xs) -> BlsqInt (read $ (show (abs a)) \\ (show (abs b))) : xs
   _ -> BlsqError "Burlesque: (\\\\) Invalid arguments!" : st

-- | r@
builtinRange :: BlsqState
builtinRange = do
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (BlsqStr a : xs) -> BlsqBlock (map BlsqStr $ subsequences a) : xs
   (BlsqBlock a : xs) -> BlsqBlock (map BlsqBlock $ subsequences a) : xs
   (BlsqInt a : xs) -> BlsqBlock (map BlsqInt [a..]) : xs
   _ -> BlsqError "Burlesque: (R@) Invalid arguments!" : st

-- | > bx
builtinBox :: BlsqState
builtinBox = do
 st <- getStack
 putResult $
  case st of
   (a : xs) -> (BlsqBlock [a]) : xs
   _ -> BlsqError "Burlesque: (bx) Invalid arguments!" : st

-- | > ><
builtinSort :: BlsqState
builtinSort = do
 st <- getStack
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
 st <- getStack
 case st of 
   (BlsqBlock [] : xs) -> putResult $ BlsqInt 0 : xs
   _ -> do pushToStack(BlsqBlock [(BlsqIdent "&&")])
           builtinReduce

-- | > r|
builtinOrLs :: BlsqState
builtinOrLs = do
 st <- getStack
 case st of 
   (BlsqBlock [] : xs) -> putResult $ BlsqInt 0 : xs
   _ -> do pushToStack(BlsqBlock [(BlsqIdent "||")])
           builtinReduce

-- | > ZZ
builtinToUpper :: BlsqState
builtinToUpper = do
 st <- getStack
 case st of 
   (BlsqChar a : xs) -> putResult $ BlsqChar (toUpper a) : xs
   (BlsqStr a : xs) -> putResult $ BlsqStr (map toUpper a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [BlsqIdent "ZZ"]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (ZZ) Invalid arguments!" : st

-- | > zz
builtinToLower :: BlsqState
builtinToLower = do
 st <- getStack
 case st of 
   (BlsqChar a : xs) -> putResult $ BlsqChar (toLower a) : xs
   (BlsqStr a : xs) -> putResult $ BlsqStr (map toLower a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [BlsqIdent "zz"]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (zz) Invalid arguments!" : st

-- | > M[
builtinMapPretty :: BlsqState
builtinMapPretty = do
 builtinMap
 builtinPretty

-- | > M]
builtinMapToPretty :: BlsqState
builtinMapToPretty = do
 pushToStack (BlsqBlock [BlsqIdent "sh"])
 builtinMap

-- | > m]
builtinMapToPrettyFromFormat :: BlsqState
builtinMapToPrettyFromFormat = do
 pushToStack (BlsqBlock [BlsqIdent "sh", BlsqIdent "ff"])
 builtinMap

-- | > [m
builtinMapDup :: BlsqState
builtinMapDup = do
 pushToStack (BlsqIdent "^^")
 builtinPrepend
 builtinMap

-- | > [M
builtinMapParse :: BlsqState
builtinMapParse = do
 pushToStack (BlsqIdent "ps")
 builtinPrepend
 builtinMap

-- | ??
builtinVersion :: BlsqState
builtinVersion = pushToStack (BlsqStr "Burlesque - 1.7.3" )

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
 st <- getStack
 case st of
  (BlsqStr _ : xs) -> do
         pushToStack (BlsqStr " " )
         builtinSplit
  (BlsqBlock [] : xs) -> putResult $ BlsqStr "" : xs
  (BlsqBlock a : xs) -> do
         pushToStack (BlsqChar ' ')
         builtinSwap
         builtinIntersperse
         builtinConcat
  _ -> do builtinSwap
          builtinIntersperse

-- | z[
builtinZip :: BlsqState
builtinZip = do
 st <- getStack
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
 st <- getStack
 case st of
   (BlsqBlock f : a : b : xs) -> do putStack $ a : b : BlsqBlock f : xs
                                    builtinZip
                                    builtinSwap
                                    builtinMap
                                    
-- | Z]
builtinZipWithPush :: BlsqState
builtinZipWithPush = do
 pushToStack (BlsqIdent "^p")
 builtinPrepend
 builtinZipWith

-- | !!
builtinBlockAccess :: BlsqState
builtinBlockAccess = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt a : BlsqBlock b : xs) -> (b !! (toInt a)) : xs
   (BlsqInt a : BlsqStr b : xs) -> BlsqChar (b !! (toInt a)) : xs
   _ -> BlsqError "Burlesque: (!!) Invalid arguments!" : st

-- | fi
builtinFindIndex :: BlsqState
builtinFindIndex = do
  st <- getStack
  case st of
   (BlsqBlock p : BlsqBlock b : xs) -> do
     fi <- findIndex' p b 0
     putResult $ (BlsqInt $ fi) : xs
   (BlsqBlock p : BlsqStr b : xs) -> builtinSwap >> builtinExplode >> 
                                     builtinSwap >> builtinFindIndex
   _ -> putResult $ BlsqError "Burlesque: (fi) Invalid arguments!" : st
 where findIndex' p [] _ = return $ -1
       findIndex' p (x:xs) i = do
                                 rs <- runStack p [x]
                                 case rs of
                                  (BlsqInt 1 : ys) -> return $ i
                                  _ -> findIndex' p xs (succ i)

-- | Fi
builtinFindIndexEq :: BlsqState
builtinFindIndexEq = do
  st <- getStack
  case st of
   (p : BlsqBlock b : xs) -> do
     fi <- findIndex' p b 0
     putResult $ (BlsqInt $ fi) : xs
   (BlsqChar p : BlsqStr b : xs) -> builtinSwap >> builtinExplode >> 
                                     builtinSwap >> builtinFindIndexEq
   _ -> putResult $ BlsqError "Burlesque: (fi) Invalid arguments!" : st
 where findIndex' p [] _ = return $ -1
       findIndex' p (x:xs) i = case x == p of
                                 True -> return $ i
                                 _ -> findIndex' p xs (succ i)

-- | fe
builtinFindElement :: BlsqState
builtinFindElement = do
  st <- getStack
  case st of
   (BlsqBlock p : BlsqBlock b : xs) -> do
     fi <- findElement' p b
     putResult $ (fi) : xs
   (BlsqBlock p : BlsqStr b : xs) -> builtinSwap >> builtinExplode >> 
                                     builtinSwap >> builtinFindElement
   _ -> putResult $ BlsqError "Burlesque: (fi) Invalid arguments!" : st
 where findElement' p [] = return $ BlsqError "Burlesque: (fe) Element not fund!"
       findElement' p (x:xs) = do
                                 rs <- runStack p [x]
                                 case rs of
                                  (BlsqInt 1 : ys) -> return $ x
                                  _ -> findElement' p xs
 
-- | CB
builtinCombinations :: BlsqState
builtinCombinations = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt n : BlsqBlock ls : xs) -> (BlsqBlock $ map (BlsqBlock) (replicateM (toInt n) ls)) : xs
   (BlsqInt n : BlsqStr ls : xs) -> (BlsqBlock $ map (BlsqStr) (replicateM (toInt n) ls)) : xs
   (BlsqInt n : BlsqInt ls : xs) -> (BlsqBlock $ map (BlsqInt . read) (replicateM (toInt n) (show.abs $ ls))) : xs
   _ -> BlsqError "Burlesque: (CB) Invalid arguments!" : st

-- | cb
builtinCombinationsUpTo :: BlsqState
builtinCombinationsUpTo  = do
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (BlsqBlock a : xs) -> (BlsqBlock (cycle a)) : xs
   (BlsqStr a : xs) -> (BlsqStr (cycle a)) : xs
   (BlsqInt a : xs) -> (BlsqStr (cycle . show . abs $ a)) : xs
   _ -> BlsqError "Burlesque: (cy) Invalid arguments!" : st

-- | is
builtinIsError :: BlsqState
builtinIsError = do
 st <- getStack
 putResult $
  case st of
   (BlsqError _ : xs) -> (BlsqInt 1) : xs
   _ -> (BlsqInt 0) : st

-- | fC
builtinPrimeFactors :: BlsqState
builtinPrimeFactors = do
 st <- getStack
 case st of
   (BlsqInt a : xs) -> putResult $ (BlsqBlock . map BlsqInt $ pfactor a) : xs
   (BlsqDouble a : xs) -> putResult $ (BlsqDouble $ a * a) : xs
   (BlsqBlock a :xs) -> do pushToStack (BlsqBlock [ BlsqIdent "fC" ]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (fC) Invalid arguments" : st

-- | fc
builtinFactors :: BlsqState
builtinFactors = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt a : xs) -> (BlsqBlock $ map (BlsqInt) . nub . sort $ factors' a a) : xs
   (BlsqStr a : xs) -> (BlsqChar $ leastCommon a) : xs
   (BlsqBlock a : xs) -> (leastCommon a) : xs

   _ -> BlsqError "Burlesque: (fc) Invalid arguments!" : st
 where factors' _ 0 = []
       factors' a b
         |fromIntegral b < (sqrt $ fromIntegral a) = []
         |a `rem` b == 0 = (a `div` b) : b : factors' a (b - 1)
         |otherwise = factors' a (b - 1)
       leastCommon :: Ord a => [a] -> a
       leastCommon = head . minimumBy (comparing length) . group . sort

-- | co
builtinChunksOf :: BlsqState
builtinChunksOf = do
 st <- getStack
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
 st <- getStack
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
 st <- getStack
 case st of
   (BlsqStr a : xs) -> putResult $ BlsqStr (dropWhile (`elem`"\t \n") a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [BlsqIdent "t["]) >> builtinMap
   _ -> putResult $ BlsqChar '\n' : st

-- | t]
builtinTrimRight :: BlsqState
builtinTrimRight = do
 st <- getStack
 case st of
   (BlsqStr a : xs) -> putResult $ BlsqStr (reverse (dropWhile (`elem`"\t \n") (reverse a))) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [BlsqIdent "t]"]) >> builtinMap
   _ -> putResult $ BlsqChar '\'' : st

builtinTrimLeftRight :: BlsqState
builtinTrimLeftRight = do
 builtinTrimLeft
 builtinTrimRight

-- | n!
builtinNot :: BlsqState
builtinNot = do
 st <- getStack
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
  st <- getStack
  case st of
   (BlsqBlock p : BlsqBlock b : xs) -> do
     qq <- findIndices' p b 0
     putResult $ BlsqBlock (map BlsqInt $ qq) : xs
   (BlsqBlock p : BlsqStr b : xs) -> builtinSwap >> builtinExplode >> 
                                     builtinSwap >> builtinFindIndices
   _ -> putResult $ BlsqError "Burlesque: (fi) Invalid arguments!" : st
 where findIndices' p [] _ = return $ []
       findIndices' p (x:xs) i = do
                                  rs <- runStack p [x]
                                  case rs of
                                   (BlsqInt 1 : ys) -> do
                                     is <- findIndices' p xs (succ i)
                                     return $ i : is
                                   _ -> findIndices' p xs (succ i) 

-- | lg
builtinLog :: BlsqState
builtinLog = do
  st <- getStack
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (log (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (log a) : xs
   _ -> putResult $ BlsqError "Burlesque: (lg) Invalid arguments!" : st

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
  st <- getStack
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (sin (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (sin a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "Ts" ]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (Ts) Invalid arguments!" : st

-- | TS
builtinAsin :: BlsqState
builtinAsin = do
  st <- getStack
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (asin (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (asin a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "TS" ]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (TS) Invalid arguments!" : st

-- | Tc
builtinCos :: BlsqState
builtinCos = do
  st <- getStack
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (cos (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (cos a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "Tc" ]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (Tc) Invalid arguments!" : st

-- | TC
builtinAcos :: BlsqState
builtinAcos = do
  st <- getStack
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (acos (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (acos a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "TC" ]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (TC) Invalid arguments!" : st

-- | Tt
builtinTan :: BlsqState
builtinTan = do
  st <- getStack
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (tan (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (tan a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "Tt" ]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (Tt) Invalid arguments!" : st

-- | TT
builtinAtan :: BlsqState
builtinAtan = do
  st <- getStack
  case st of
   (BlsqInt a : xs) -> putResult $ BlsqDouble (atan (fromIntegral a)) : xs
   (BlsqDouble a : xs) -> putResult $ BlsqDouble (atan a) : xs
   (BlsqBlock a : xs) -> pushToStack (BlsqBlock [ BlsqIdent "TT" ]) >> builtinMap
   _ -> putResult $ BlsqError "Burlesque: (TT) Invalid arguments!" : st

-- | WD
builtinWords2 :: BlsqState
builtinWords2 = do
  st <- getStack
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
  st <- getStack
  case st of
    (BlsqInt idx : e : BlsqBlock ls : xs) -> putResult $ (BlsqBlock (Burlesque.Helpers.insertAt idx e ls)) : xs
    (BlsqInt idx : BlsqChar e : BlsqStr ls : xs) -> putResult $ (BlsqStr (Burlesque.Helpers.insertAt idx e ls)) : xs
    _ -> putResult $ BlsqError "Burlesque: (ia) Invalid arguments!" : st

-- | RA
builtinRemoveAt :: BlsqState
builtinRemoveAt = do
  st <- getStack
  case st of
    (BlsqInt idx : BlsqBlock ls : xs) -> putResult $ (BlsqBlock (removeAt idx ls)) : xs
    (BlsqInt idx : BlsqStr ls : xs) -> putResult $ (BlsqStr (removeAt idx ls)) : xs
    (BlsqBlock ls : xs) -> do
          builtinSetAt
          builtinProduct
          pushToStack (BlsqDouble 2.0)
          builtinDiv
          builtinAverage
          builtinBlockAccess
    _ -> putResult $ BlsqError "Burlesque: (ra) Invalid arguments!" : st

-- | sa
builtinSetAt :: BlsqState
builtinSetAt = do
  st <- getStack
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

-- | rs
builtinRunStack :: BlsqState
builtinRunStack = do
  st <- getStack
  case st of
    (BlsqBlock f : BlsqBlock q : xs) -> do
      let st' = runStack' f q
      putStack xs
      pushToStack (BlsqBlock st')
      
-- | RS
builtinRunStack2 :: BlsqState
builtinRunStack2 = do
  builtinSwap
  builtinBox
  builtinSwap
  builtinRunStack
  
-- | gB
builtinGroupBy2 :: BlsqState
builtinGroupBy2 = do
  st <- getStack
  case st of
    (BlsqBlock f : BlsqBlock ls : xs) -> putResult $ BlsqBlock (map BlsqBlock (
                                         groupBy (\ a b -> 
                                                           (case runStack' f [a] of
                                                                 (BlsqInt 1 : xs) -> True
                                                                 (BlsqInt (-1) : xs) -> False
                                                                 _ -> False) ==
                                                            (case runStack' f [b] of
                                                                 (BlsqInt 1 : xs) -> True
                                                                 (BlsqInt (-1) : xs) -> False
                                                                 _ -> False))
                                                                 ls)) : xs
    (BlsqBlock f : BlsqStr ls : xs) -> putResult $ BlsqBlock (map BlsqStr (
                                         groupBy (\ a b -> 
                                                           (case runStack' f [BlsqChar a] of
                                                                 (BlsqInt 1 : xs) -> True
                                                                 (BlsqInt (-1) : xs) -> False
                                                                 _ -> False) ==
                                                            (case runStack' f [BlsqChar b] of
                                                                 (BlsqInt 1 : xs) -> True
                                                                 (BlsqInt (-1) : xs) -> False
                                                                 _ -> False))
                                                                 ls)) : xs
    _ -> putResult $ BlsqError "Burlesque: (gB) Invalid arguments!" : st
  
-- | gb
builtinGroupBy :: BlsqState
builtinGroupBy = do
  st <- getStack
  case st of
    (BlsqBlock f : BlsqBlock ls : xs) -> putResult $ BlsqBlock (map BlsqBlock (
                                         groupBy (\ a b -> case runStack' f [b,a] of
                                                                 (BlsqInt 1 : xs) -> True
                                                                 (BlsqInt (-1) : xs) -> False
                                                                 _ -> False) ls)) : xs
    (BlsqBlock f : BlsqStr ls : xs) -> putResult $ BlsqBlock (map BlsqStr (
                                         groupBy (\ a b -> case runStack' f [BlsqChar b,BlsqChar a] of
                                                                 (BlsqInt 1 : xs) -> True
                                                                 (BlsqInt (-1) : xs) -> False
                                                                 _ -> False) ls)) : xs
    _ -> putResult $ BlsqError "Burlesque: (gb) Invalid arguments!" : st
    
-- | sb
builtinSortBy :: BlsqState
builtinSortBy = do
  st <- getStack
  case st of
    (BlsqBlock f : BlsqBlock ls : xs) -> putResult $ BlsqBlock (
                                         sortBy (\ a b -> case runStack' f [b,a] of
                                                                 (BlsqInt 1 : xs) -> GT
                                                                 (BlsqInt (-1) : xs) -> LT
                                                                 _ -> EQ) ls) : xs
    (BlsqBlock f : BlsqStr ls : xs) -> putResult $ BlsqStr (
                                       sortBy (\ a b -> case runStack' f [BlsqChar b,BlsqChar a] of
                                                                 (BlsqInt 1 : xs) -> GT
                                                                 (BlsqInt (-1) : xs) -> LT
                                                                 _ -> EQ) ls) : xs
    _ -> putResult $ BlsqError "Burlesque: (sb) Invalid arguments!" : st

-- | cm
builtinCompare :: BlsqState
builtinCompare = do
  st <- getStack
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
  pushToStack (BlsqIdent "\\/")
  builtinAppend
  builtinSwap
  builtinAppend
  pushToStack (BlsqIdent "\\/")
  builtinAppend
  pushToStack (BlsqIdent "cm")
  builtinAppend
  
-- | Cm
builtinCompare3 :: BlsqState
builtinCompare3 = do
  -- ^^(\/)[+\/.+{\/cm}.+
  builtinDup
  pushToStack (BlsqIdent "\\/")
  builtinAppend
  builtinSwap
  builtinAdd
  pushToStack (BlsqBlock [ BlsqIdent "\\/", BlsqIdent "cm" ])
  builtinAdd

-- | B!
builtinConvertBase :: BlsqState
builtinConvertBase = do
  st <- getStack
  case st of
    (BlsqInt bs : BlsqInt n : xs) -> putResult $ BlsqStr (toBase bs n) : xs
    (BlsqInt bs : BlsqStr n : xs) -> putResult $ BlsqInt ((fromBase bs n)) : xs
    (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                         builtinBoxCycle
                                         builtinSwap
                                         builtinConvertBase
    (BlsqInt a : BlsqBlock b : xs) -> do builtinBoxCycle
                                         builtinConvertBase
    (BlsqBlock a : BlsqStr b : xs) -> do builtinSwap
                                         builtinBoxCycle
                                         builtinSwap
                                         builtinConvertBase
    (BlsqStr a : BlsqBlock b : xs) -> do builtinBoxCycle
                                         builtinConvertBase
    (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "B!" ])
                                           builtinZipWithPush
    _ -> putResult $ BlsqError "Burlesque: (B!) Invalid arguments!" : st

-- | g_
builtinGcd :: BlsqState
builtinGcd = do
  st <- getStack
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
  st <- getStack
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
 st <- getStack
 case st of
   (BlsqBlock ls : BlsqBlock p : xs) -> do 
     tw <- takeWhile' p ls
     putResult $ (BlsqBlock $ tw) : xs
   (BlsqStr ls : BlsqBlock p : xs) -> do builtinExplode
                                         builtinTakeWhile
                                         builtinConcat
   _ -> putResult $ BlsqError "Burlesque: (tw) Invalid arguments!" : st
 where takeWhile' _ [] = return []
       takeWhile' p (y:ys) = do
                              rs <- runStack p [y]
                              case rs of
                               (BlsqInt 0 : _) -> return []
                               _ -> do
                                 yy <- takeWhile' p ys
                                 return $ y : yy

-- |dw
builtinDropWhile :: BlsqState
builtinDropWhile = do
 st <- getStack
 case st of
   (BlsqBlock ls : BlsqBlock p : xs) -> do 
     dw <- dropWhile' p ls
     putResult $ (BlsqBlock $ dw) : xs
   (BlsqStr ls : BlsqBlock p : xs) -> do builtinExplode
                                         builtinDropWhile
                                         builtinConcat
   _ -> putResult $ BlsqError "Burlesque: (dw) Invalid arguments!" : st
 where dropWhile' _ [] = return []
       dropWhile' p yss@(y:ys) = do
                             rs <- runStack p [y]
                             case rs of
                               (BlsqInt 0 : _) -> return yss
                               _ -> dropWhile' p ys

-- | tp
builtinTranspose :: BlsqState
builtinTranspose = do 
 st <- getStack
 case st of
  (BlsqBlock a : xs) -> putResult $ BlsqBlock (map BlsqBlock (transpose (map (toList') a))) : xs
  _ -> putResult $  BlsqError "You should not transpose what you can't transpose. Yes this is an easteregg!" : st
 where toList' (BlsqBlock a) = a
       toList' x = [x]

-- | FM
builtinFilterMap :: BlsqState
builtinFilterMap = do
 st <- getStack
 case st of
  (BlsqBlock m : BlsqBlock f : xs) -> do builtinPop
                                         builtinPop
                                         pushToStack (BlsqBlock f)
                                         builtinFilter
                                         pushToStack (BlsqBlock m)
                                         builtinMap
  _ -> putResult $ BlsqError "Burlesque: (FM) Invalid arguments!" : st

-- | r\
builtinRangeConcat :: BlsqState
builtinRangeConcat = do builtinRange
                        builtinConcat

-- | SP
builtinSpecialInput :: BlsqState
builtinSpecialInput = do 
 st <- getStack
 case st of
  (BlsqStr s : xs) -> do builtinLines
                         pushToStack (BlsqBlock [])
                         builtinMapParse
  (BlsqBlock l : xs) -> do pushToStack (BlsqBlock [ BlsqBlock [ BlsqIdent "Sh"], BlsqIdent "m[", BlsqIdent "wd" ])
                           builtinMap
                           builtinUnlines
  _ -> putResult $ BlsqError "Burlesque: (SP) Invalid arguments!" : st

-- | hd
-- Unlike high definition this is a gruesome hack. or feature. let's say feature.
builtinHide :: BlsqState
builtinHide = do
 st <- getStack
 case st of
   (a : xs) -> do builtinPop
                  pushToBottom (BlsqHiddenState a)
   _ -> putResult $ BlsqError "Burlesque: (hd) Invalid arguments!" : st

-- | HD
-- also hackish.
builtinHide2 :: BlsqState
builtinHide2 = do
 st <- getStack
 case st of
   (a : xs) -> do builtinPop
                  pushToStack (BlsqHiddenState a)
   _ -> putResult $ BlsqError "Burlesque: (HD) Invalid arguments!" : st
   
-- | ld
-- very hackish
builtinLoad :: BlsqState
builtinLoad = do
 st <- getStack
 case st of
   (BlsqInt a : xs) -> do builtinPop
                          let hidden = st !! (toInt (length st - 1 - (toInt a)))
                          case hidden of
                            (BlsqHiddenState hstate) -> do pushToStack(hstate)
                            _ -> do pushToStack (BlsqError "Can't load non hidden state! Sorry.")
   _ -> putResult $ BlsqError "Burlesque: (ld) Invalid arguments!" : st

-- | LD
builtinLoad2 :: BlsqState
builtinLoad2 = do builtinLoad
                  builtinEval

-- | st
builtinStore :: BlsqState
builtinStore = do
 st <- getStack
 case st of
   (BlsqInt a : e : xs) -> do putStack $ setAt (toInt (length xs - 1 - (toInt a))) (BlsqHiddenState e) xs
   _ -> putResult $ BlsqError "Burlesque: (st) Invalid arguments!" : st

builtinALoad = pushToStack (BlsqInt 0) >> builtinLoad
builtinBLoad = pushToStack (BlsqInt 1) >> builtinLoad
builtinCLoad = pushToStack (BlsqInt 2) >> builtinLoad
builtinALoad2 = pushToStack (BlsqInt 0) >> builtinLoad2
builtinBLoad2 = pushToStack (BlsqInt 1) >> builtinLoad2
builtinCLoad2 = pushToStack (BlsqInt 2) >> builtinLoad2
builtinAStore = pushToStack (BlsqInt 0) >> builtinStore
builtinBStore = pushToStack (BlsqInt 1) >> builtinStore
builtinCStore = pushToStack (BlsqInt 2) >> builtinStore

-- | sc
builtinSortByComparing :: BlsqState
builtinSortByComparing = do
 builtinCompare2
 builtinSortBy

-- | SC
builtinSortByComparing2 :: BlsqState
builtinSortByComparing2 = do
 builtinCompare3
 builtinSortBy

-- | ?+
builtinCoerceAdd :: BlsqState
builtinCoerceAdd = do
 st <- getStack
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
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?+" ])
                                          builtinZipWith
   _ -> builtinAdd

-- | ?-
builtinCoerceSub :: BlsqState
builtinCoerceSub = do
 st <- getStack
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
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?-" ])
                                          builtinZipWith
   _ -> builtinSub

-- | ?/
builtinCoerceDiv :: BlsqState
builtinCoerceDiv = do
 st <- getStack
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
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?/" ])
                                          builtinZipWith
   _ -> builtinDiv

-- | ?*
builtinCoerceMul :: BlsqState
builtinCoerceMul = do
 st <- getStack
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
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?*" ])
                                          builtinZipWith
   _ -> builtinMul
   
-- | ?^
builtinCoercePow :: BlsqState
builtinCoercePow = do
 st <- getStack
 case st of
   (BlsqInt a : BlsqDouble b : xs) -> do builtinProduct
                                         builtinPow
   (BlsqDouble a : BlsqInt b : xs) -> do builtinSwap
                                         builtinProduct
                                         builtinSwap
                                         builtinPow
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBox
                                        builtinCycle
                                        builtinSwap
                                        builtinCoercePow
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBox
                                        builtinCycle
                                        builtinCoercePow
   (BlsqBlock a : BlsqDouble b : xs) -> do builtinSwap
                                           builtinBox
                                           builtinCycle
                                           builtinSwap
                                           builtinCoercePow
   (BlsqDouble a : BlsqBlock b : xs) -> do builtinBox
                                           builtinCycle
                                           builtinCoercePow
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "^p", BlsqIdent "?^" ])
                                          builtinZipWith
   _ -> builtinPow
   
-- | ?s
builtinCoerceSqrt :: BlsqState
builtinCoerceSqrt = do
 st <- getStack
 case st of
   (BlsqInt a : xs) -> do builtinProduct
                          builtinRange
   (BlsqBlock a : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "?s" ])
                            builtinMap
   _ -> builtinRange
   
-- | ?!
builtinCoerceFactorial :: BlsqState
builtinCoerceFactorial = do
 st <- getStack
 case st of
   (BlsqInt a : xs) -> do builtinRangeFromOne
                          builtinProduct
   (BlsqBlock a : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "?!" ])
                            builtinMap
   _ -> do builtinRangeFromOne
           builtinProduct
           
-- | ?i
builtinCoerceInc :: BlsqState
builtinCoerceInc = do
 st <- getStack
 case st of
   (BlsqDouble a : xs) -> do pushToStack (BlsqDouble 1.0)
                             builtinCoerceAdd
   (BlsqBlock a : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "?i" ])
                            builtinMap
   _ -> builtinIncrement
   
-- | ?d
builtinCoerceDec :: BlsqState
builtinCoerceDec = do
 st <- getStack
 case st of
   (BlsqDouble a : xs) -> do pushToStack (BlsqDouble 1.0)
                             builtinCoerceSub
   (BlsqBlock a : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "?d" ])
                            builtinMap
   _ -> builtinDecrement
   
-- | im
builtinImplode :: BlsqState
builtinImplode = do 
 pushToStack (BlsqBlock [BlsqIdent "++"])
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
 st <- getStack
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
 pushToStack (BlsqIdent "!!")
 builtinSwap
 builtinIntersperse
 pushToStack (BlsqIdent "!!")
 builtinAppend
 builtinEval
 
-- | D!
builtinDimArraySet :: BlsqState
builtinDimArraySet = do
 --{0 1 1} -> ^^0!!^^1!!8 1sa1 sa0 sa
 st <- getStack
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
 pushToStack(BlsqIdent "Sh")
 builtinAppend
 builtinMap
 
-- | Wl
builtinWithLinesString :: BlsqState
builtinWithLinesString = do
 pushToStack(BlsqIdent "Sh")
 builtinAppend
 builtinWithLinesPretty
 
-- | si
builtinSelectIndices :: BlsqState
builtinSelectIndices = do
 -- {1 0 3}{"ABCD"\/!!}m[
 st <- getStack
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
 pushToStack (BlsqInt 1)
 builtinSwap
 builtinRange
 
-- | rz
builtinRangeFromZero :: BlsqState
builtinRangeFromZero = do
 pushToStack (BlsqInt 0)
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
 st <- getStack
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
 st <- getStack
 putResult $
  case st of
   (BlsqStr s : BlsqStr rgx : xs) -> BlsqBlock (map BlsqStr . splitRegex (mkRegex rgx) $ s) : xs
   _ -> BlsqError "Burlesque: (sr) Invalid arguments!" : st
   
-- | rn
builtinRandomInts :: BlsqState
builtinRandomInts = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt ho : BlsqInt lo : BlsqInt seed : xs) -> (BlsqBlock . map BlsqInt $ randomRs (lo,ho) (mkStdGen (toInt seed))) : xs
   _ -> BlsqError "Burlesque: (rn) Invalid arguments!" : st
   
-- | RN
builtinRandomDoubles :: BlsqState
builtinRandomDoubles = do
 st <- getStack
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
 pushToStack (BlsqBlock [BlsqIdent "~="])
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
 
-- | ch
builtinChoose :: BlsqState
builtinChoose = do
 builtinSwap
 builtinNot
 builtinBlockAccess
 
-- | f~
builtinSimpleFormat :: BlsqState
builtinSimpleFormat = do
 -- \/"~";;\/{Sh}m[**\[
 builtinSwap
 pushToStack (BlsqStr "~")
 builtinSplit
 builtinSwap
 pushToStack (BlsqBlock [ BlsqIdent "Sh" ])
 builtinMap
 builtinPow
 builtinConcat
 
-- | pi
builtinPi :: BlsqState
builtinPi = do 
 pushToStack (BlsqDouble pi)
 
-- |  ee
builtinE :: BlsqState
builtinE = do
 pushToStack (BlsqDouble (exp 1))
 
-- | rm
builtinRangeModulo :: BlsqState
builtinRangeModulo = do
 st <- getStack
 putResult $ 
  case st of
   (BlsqBlock [BlsqInt lo, BlsqInt hi] : BlsqInt x : xs) -> BlsqInt (((x - lo) `mod` (succ (hi-lo))) + lo) : xs
   (BlsqInt n : BlsqDouble d : xs) -> (BlsqStr $ showFFloat (Just (toInt n)) (d) "") : xs
   _ -> BlsqError "Burlesque: (rm) Invalid arguments!" : st
 
-- | nc
builtinNormalDCumulative :: BlsqState
builtinNormalDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble std : BlsqDouble mn : xs) -> BlsqDouble (cumulative (normalDistr mn std) ho) : xs
   _ -> BlsqError "Burlesque: (nc) Invalid arguments!" : st
   
-- | nq
builtinNormalDQuantile :: BlsqState
builtinNormalDQuantile = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble std : BlsqDouble mn : xs) -> BlsqDouble (quantile (normalDistr mn std) ho) : xs
   _ -> BlsqError "Burlesque: (nq) Invalid arguments!" : st
   
-- | nd
builtinNormalDDensity :: BlsqState
builtinNormalDDensity = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble std : BlsqDouble mn : xs) -> BlsqDouble (density (normalDistr mn std) ho) : xs
   _ -> BlsqError "Burlesque: (nd) Invalid arguments!" : st
   
-- | Bc
builtinBinomialDCumulative :: BlsqState
builtinBinomialDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble p : BlsqInt n : xs) -> BlsqDouble (cumulative (binomial (toInt n) p) ho) : xs
   _ -> BlsqError "Burlesque: (Bc) Invalid arguments!" : st
   
-- | Bp
builtinBinomialDProbability :: BlsqState
builtinBinomialDProbability = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt ho : BlsqDouble p : BlsqInt n : xs) -> BlsqDouble (probability (binomial (toInt n) p) (toInt ho)) : xs
   _ -> BlsqError "Burlesque: (Bp) Invalid arguments!" : st
   
-- | pc
builtinPoissonDCumulative :: BlsqState
builtinPoissonDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lambda :xs) -> BlsqDouble (cumulative (poisson (lambda)) ho) : xs
   _ -> BlsqError "Burlesque: (pc) Invalid arguments!" : st
   
-- | pp
builtinPoissonDProbability :: BlsqState
builtinPoissonDProbability = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt ho : BlsqDouble lambda : xs) -> BlsqDouble (probability (poisson (lambda)) (toInt ho)) : xs
   _ -> BlsqError "Burlesque: (pp) Invalid arguments!" : st
   
-- | gc
builtinGeometricDCumulative :: BlsqState
builtinGeometricDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble suc :xs) -> BlsqDouble (cumulative (geometric (suc)) ho) : xs
   _ -> BlsqError "Burlesque: (gc) Invalid arguments!" : st
   
-- | gp
builtinGeometricDProbability :: BlsqState
builtinGeometricDProbability = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt ho : BlsqDouble suc : xs) -> BlsqDouble (probability (geometric (suc)) (toInt ho)) : xs
   _ -> BlsqError "Burlesque: (gp) Invalid arguments!" : st
   
-- | hc
builtinHypergeometricDCumulative :: BlsqState
builtinHypergeometricDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqInt k : BlsqInt l : BlsqInt m : xs) -> BlsqDouble (cumulative (hypergeometric (toInt m) (toInt l) (toInt k)) ho) : xs
   _ -> BlsqError "Burlesque: (hc) Invalid arguments!" : st
   
-- | hp
builtinHypergeometricDProbability :: BlsqState
builtinHypergeometricDProbability = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt ho : BlsqInt k : BlsqInt l : BlsqInt m : xs) -> BlsqDouble (probability (hypergeometric (toInt m) (toInt l) (toInt k)) (toInt ho)) : xs
   _ -> BlsqError "Burlesque: (hp) Invalid arguments!" : st
   
-- | cc
builtinChiSquaredDCumulative :: BlsqState
builtinChiSquaredDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqInt df : xs) -> BlsqDouble (cumulative (chiSquared (toInt df)) ho) : xs
   _ -> BlsqError "Burlesque: (cc) Invalid arguments!" : st
   
-- | cd
builtinChiSquaredDDensity :: BlsqState
builtinChiSquaredDDensity = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqInt df : xs) -> BlsqDouble (density (chiSquared (toInt df)) ho) : xs
   _ -> BlsqError "Burlesque: (cd) Invalid arguments!" : st
   
-- | cq
builtinChiSquaredDQuantile :: BlsqState
builtinChiSquaredDQuantile = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqInt df : xs) -> BlsqDouble (quantile (chiSquared (toInt df)) ho) : xs
   _ -> BlsqError "Burlesque: (cq) Invalid arguments!" : st
   
-- | ec
builtinExponentialDCumulative :: BlsqState
builtinExponentialDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lambda :xs) -> BlsqDouble (cumulative (exponential (lambda)) ho) : xs
   _ -> BlsqError "Burlesque: (ec) Invalid arguments!" : st
   
-- | ed
builtinExponentialDDensity :: BlsqState
builtinExponentialDDensity = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lambda : xs) -> BlsqDouble (density (exponential (lambda)) (ho)) : xs
   _ -> BlsqError "Burlesque: (ed) Invalid arguments!" : st
   
-- | eq
builtinExponentialDQuantile :: BlsqState
builtinExponentialDQuantile = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lambda : xs) -> BlsqDouble (quantile (exponential (lambda)) (ho)) : xs
   _ -> BlsqError "Burlesque: (eq) Invalid arguments!" : st
   
-- | Sc
builtinStudentTDCumulative :: BlsqState
builtinStudentTDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lambda :xs) -> BlsqDouble (cumulative (studentT (lambda)) ho) : xs
   _ -> BlsqError "Burlesque: (Sc) Invalid arguments!" : st
   
-- | Sd
builtinStudentTDDensity :: BlsqState
builtinStudentTDDensity = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lambda : xs) -> BlsqDouble (density (studentT (lambda)) (ho)) : xs
   _ -> BlsqError "Burlesque: (Sd) Invalid arguments!" : st
   
-- | Sq
builtinStudentTDQuantile :: BlsqState
builtinStudentTDQuantile = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble lambda : xs) -> BlsqDouble (quantile (studentT (lambda)) (ho)) : xs
   _ -> BlsqError "Burlesque: (Sq) Invalid arguments!" : st
   
-- | uc
builtinUniformDCumulative :: BlsqState
builtinUniformDCumulative = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble high : BlsqDouble low :xs) -> BlsqDouble (cumulative (uniformDistr low high) ho) : xs
   _ -> BlsqError "Burlesque: (uc) Invalid arguments!" : st
   
-- | ud
builtinUniformDDensity :: BlsqState
builtinUniformDDensity = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble high : BlsqDouble low : xs) -> BlsqDouble (density (uniformDistr low high) (ho)) : xs
   _ -> BlsqError "Burlesque: (ud) Invalid arguments!" : st
   
-- | uq
builtinUniformDQuantile :: BlsqState
builtinUniformDQuantile = do
 st <- getStack
 putResult $
  case st of
   (BlsqDouble ho : BlsqDouble high : BlsqDouble low : xs) -> BlsqDouble (quantile (uniformDistr low high) (ho)) : xs
   _ -> BlsqError "Burlesque: (uq) Invalid arguments!" : st
   
-- | f:
builtinFrequencyList :: BlsqState
builtinFrequencyList = do
 -- sg{^^L[\/-]bx\/+]}m[<>
 builtinSortGroup
 pushToStack( BlsqBlock [
      BlsqIdent "^^",
      BlsqIdent "L[",
      BlsqIdent "\\/",
      BlsqIdent "-]",
      BlsqIdent "bx",
      BlsqIdent "\\/",
      BlsqIdent "+]"
    ])
 builtinMap
 builtinSortReverse
 
-- | F:
builtinFrequencyListPercentage :: BlsqState
builtinFrequencyListPercentage = do
 -- f:u[\/PD^^++?/\/z[
 builtinFrequencyList
 builtinUnzip
 builtinSwap
 builtinProductMany
 builtinDup
 builtinSum
 builtinCoerceDiv
 builtinSwap
 builtinZip
 
-- | u[
builtinUnzip :: BlsqState
builtinUnzip = do
 -- ^^{-]}m[\/{[~}m[
 builtinDup
 pushToStack (BlsqBlock [ BlsqIdent "-]"])
 builtinMap
 builtinSwap
 pushToStack (BlsqBlock [ BlsqIdent "[~" ])
 builtinMap
 
-- | U[
builtinUngroup :: BlsqState
builtinUngroup = do
 --{p^.*}\m
 pushToStack ( BlsqBlock [ BlsqIdent "p^" , BlsqIdent ".*" ])
 builtinConcatMap
 
-- | vr
builtinVariance :: BlsqState
builtinVariance = do
 -- ^^^^avbx(?-)[+m[2?^++\/L[-.?/
 builtinDup
 builtinDup
 builtinAverage
 builtinBox
 pushToStack (BlsqIdent "?-")
 builtinAppend
 builtinMap
 pushToStack (BlsqInt 2)
 builtinCoercePow
 builtinSum
 builtinSwap
 builtinLength
 builtinDecrement
 builtinCoerceDiv
 
-- | SD
builtinStandardDeviation :: BlsqState
builtinStandardDeviation = do
 builtinVariance
 builtinCoerceSqrt
 
-- | x/
builtinXSwap :: BlsqState
builtinXSwap = do
 st <- getStack
 case st of 
  (c : b : a : xs) -> putResult $ a : c : b : xs
  _ -> putResult $ BlsqError "Burlesque: (x/) Stack size error!" : st
  
-- | ct
builtinChiSquaredTest :: BlsqState
builtinChiSquaredTest = do
 -- ^^x/?-2?^\/0.0?+?/++
 builtinDup
 builtinXSwap
 builtinCoerceSub
 pushToStack (BlsqInt 2)
 builtinCoercePow
 builtinSwap
 pushToStack (BlsqDouble 0.0)
 builtinCoerceAdd
 builtinCoerceDiv
 builtinSum
 
-- | nr
builtinNCr :: BlsqState
builtinNCr = do
 st <- getStack
 case st of
   (BlsqInt k : BlsqInt n : xs) -> putResult $ BlsqInt (ncr n k) : xs
   (BlsqBlock a : BlsqInt b : xs) -> do builtinSwap
                                        builtinBoxCycle
                                        builtinSwap
                                        builtinNCr
   (BlsqInt a : BlsqBlock b : xs) -> do builtinBoxCycle
                                        builtinNCr
   (BlsqBlock a : BlsqBlock b : xs) -> do pushToStack (BlsqBlock [ BlsqIdent "nr" ])
                                          builtinZipWithPush
   _ -> putResult $ BlsqError "Burlesque: (nr) Invalid arguments!" : st
 where ncr n k = product [k+1..n] `div` product [1..n-k]
 
-- | zi
builtinZipIndices :: BlsqState
builtinZipIndices = do
 -- 0R@\/z[
 pushToStack (BlsqInt 0)
 builtinRangeInf
 builtinSwap
 builtinZip
 
-- | al
builtinAll :: BlsqState
builtinAll = do
 builtinMap
 builtinAndLs
 
-- | ay
builtinAny :: BlsqState
builtinAny = do
 builtinMap
 builtinOrLs
 
-- | ad
builtinAllDigit :: BlsqState
builtinAllDigit = do
 pushToStack (BlsqBlock [BlsqIdent "><"])
 builtinAll
 
-- | an
builtinAllAlphaNum :: BlsqState
builtinAllAlphaNum = do
 pushToStack (BlsqBlock [BlsqIdent "ri"])
 builtinAll

-- | aa
builtinAllAlpha :: BlsqState
builtinAllAlpha = do
 pushToStack (BlsqBlock [BlsqIdent "rd"])
 builtinAll
 
-- | w[
builtinFilterWords :: BlsqState
builtinFilterWords = do 
 -- \/WD\/f[wd
 builtinSwap
 builtinWords2
 builtinSwap
 builtinFilter
 builtinWords
 
-- | W[
builtinFilterLines :: BlsqState
builtinFilterLines = do
 -- \/ln\/f[un
 builtinSwap
 builtinLines
 builtinSwap
 builtinFilter
 builtinUnlines
 
-- | so
builtinSorted :: BlsqState
builtinSorted = do
 builtinDup
 builtinSort
 builtinEqual
 
-- | SO
builtinSortedReverse :: BlsqState
builtinSortedReverse = do
 builtinDup
 builtinSortReverse
 builtinEqual
 
-- | ic
builtinIntercalate :: BlsqState
builtinIntercalate = do
 builtinIntersperse
 builtinConcat
 
-- | =s
builtinSortEqual :: BlsqState
builtinSortEqual = do
 builtinSort
 builtinSwap
 builtinSort
 builtinEqual
 
-- | pt
builtinPartition :: BlsqState
builtinPartition = do
 st <- getStack
 case st of
  (BlsqBlock p : BlsqStr s : xs) -> do
    builtinSwap
    builtinExplode
    builtinSwap
    builtinPartition
    pushToStack (BlsqBlock [BlsqIdent "\\[",BlsqIdent "es"])
    builtinMap
  (BlsqBlock p : BlsqBlock s : xs) -> do
    builtinFilter
    builtinBox
    pushToStack (BlsqBlock s)
    pushToStack (BlsqBlock (p ++ [BlsqIdent "n!"]))
    builtinFilter
    builtinBox
    builtinAdd

-- | es
builtinEmptyBlockToStr :: BlsqState
builtinEmptyBlockToStr = do
 st <- getStack
 case st of
  (BlsqBlock [] : xs) -> putResult $ BlsqStr "" : xs
  _ -> return ()
  
-- | s=
builtinStrEqIgnoreCase :: BlsqState
builtinStrEqIgnoreCase = do
 builtinToLower
 builtinSwap
 builtinToLower
 builtinSwap
 builtinEqual
 
-- | gw
builtinGroupWithLength :: BlsqState
builtinGroupWithLength = do
 -- =[{^^L[\/-]bx\/+]}m[
 builtinGroup
 pushToStack( BlsqBlock [
      BlsqIdent "^^",
      BlsqIdent "L[",
      BlsqIdent "\\/",
      BlsqIdent "-]",
      BlsqIdent "bx",
      BlsqIdent "\\/",
      BlsqIdent "+]"
    ])
 builtinMap
 
-- | gl
builtinGroupLength :: BlsqState
builtinGroupLength = do
 builtinGroup
 builtinLength
 
-- | gn
builtinGroupNub :: BlsqState
builtinGroupNub = do
 -- =[{-]}m[
 builtinGroup
 pushToStack (BlsqBlock [BlsqIdent "-]"])
 builtinMap
 
-- | mo
builtinMultiplesOf :: BlsqState
builtinMultiplesOf = do
 -- 1R@\/?*
 pushToStack (BlsqInt 1)
 builtinRangeInf
 builtinSwap
 builtinCoerceMul
 

-- | mm
builtinMmult :: BlsqState
builtinMmult = do
 builtinTranspose
 st <- getStack
 case st of
  (BlsqBlock b : BlsqBlock a : xs) -> do
    putResult $ (BlsqBlock . map BlsqBlock $ [ [ qsum $ qmul ar bc | bc <- b ] | ar <- a ]) : xs
 where qmul a b = head . fst' $ 
        execState
         (do pushToStack (a)
             pushToStack (b)
             pushToStack (BlsqBlock [ BlsqIdent "?*" ])
             builtinZipWithPush) ([],[], M.fromList [])
       qsum ls = head . fst' $
        execState
         (do pushToStack (ls)
             builtinSum) ([],[], M.fromList [])

-- | ss
builtinStrStr :: BlsqState
builtinStrStr = do
 -- ^^L[x/\/CO\/Fi
 builtinDup
 builtinLength
 builtinXSwap
 builtinSwap
 builtinChunky
 builtinSwap
 builtinFindIndexEq
 
-- | en
builtinEveryNth :: BlsqState
builtinEveryNth = do
 st <- getStack
 case st of
   (BlsqInt n : BlsqBlock _ : xs) -> do 
      -- co{[~}m[
      builtinPop
      builtinZipIndices
      pushToStack (BlsqBlock [ BlsqIdent "-]", BlsqIdent "?i", BlsqInt n, BlsqIdent ".%", BlsqIdent "n!" ])
      builtinFilter
      builtinUnzip
      builtinSwapPop
   (BlsqInt n : BlsqStr _ : xs) -> do
      builtinPop
      builtinZipIndices
      pushToStack (BlsqBlock [ BlsqIdent "-]", BlsqIdent "?i", BlsqInt n, BlsqIdent ".%", BlsqIdent "n!" ])
      builtinFilter
      builtinUnzip
      builtinSwapPop
      builtinConcat
   (BlsqInt _ : BlsqInt _ : xs) -> do
      builtinSwap
      builtinExplode
      builtinSwap
      builtinEveryNth
      builtinImplode
      
-- | pe
builtinParseEval :: BlsqState
builtinParseEval = do
 builtinParse
 builtinEval
 
-- | sl
builtinSelectLines :: BlsqState
builtinSelectLines = do
 builtinSwap
 builtinLines
 builtinSwap
 builtinSelectIndices
 builtinUnlines
 
-- | sw
builtinSelectWords :: BlsqState
builtinSelectWords = do
 builtinSwap
 builtinWords2
 builtinSwap
 builtinSelectIndices
 builtinWords
 
-- | di
builtinDeleteIndices :: BlsqState
builtinDeleteIndices = do
 -- (RA)\/[[(RA)[+e!
 pushToStack(BlsqIdent "RA")
 builtinSwap
 builtinIntersperse
 pushToStack(BlsqIdent "RA")
 builtinAppend
 builtinEval
 
-- | tl
builtinTrimLines :: BlsqState
builtinTrimLines = do
 builtinLines
 pushToStack (BlsqBlock [ BlsqIdent "tt" ])
 builtinMap
 builtinUnlines
 
-- | sp
builtinSpecialInputPretty :: BlsqState
builtinSpecialInputPretty = do
 builtinSpecialInput
 builtinPretty
 
-- | td
builtinToDouble :: BlsqState
builtinToDouble = do
 st <- getStack
 case st of
   (BlsqDouble a : xs) -> return ()
   (BlsqStr a : xs) -> builtinReadDouble
   (BlsqInt a : xs) -> builtinProduct
   _ -> putResult $ BlsqError "Burlesque: (td) Invalid arguments!" : st
   
-- | ti
builtinToInt :: BlsqState
builtinToInt = do
 st <- getStack
 case st of
   (BlsqDouble a : xs) -> builtinProduct
   (BlsqStr a : xs) -> builtinReadInt
   (BlsqInt a : xs) -> return ()
   (BlsqBlock a : xs) -> builtinImplode
   _ -> putResult $ BlsqError "Burlesque: (td) Invalid arguments!" : st
   
-- | su
builtinSubstrings :: BlsqState
builtinSubstrings = do
 st <- getStack
 case st of
   (BlsqStr s : xs) -> putResult $ (BlsqBlock . map BlsqStr $ genSubstrings s) : xs
   (BlsqBlock s : xs) -> putResult $ (BlsqBlock . map BlsqBlock  $ genSubstrings s) : xs
   (BlsqInt s : xs) -> putResult $ (BlsqBlock . map (BlsqInt . read) $ genSubstrings (show . abs$s)) : xs

  
-- | #s
builtinPushStack :: BlsqState
builtinPushStack = do
 st <- getStack
 pushToStack (BlsqBlock st)
 
-- | #S
builtinPopStack :: BlsqState
builtinPopStack = do
 st <- getStack
 case st of
   (BlsqBlock x:xs) -> putStack $ x 
   _ -> return ()

-- | #r
builtinRotateStackLeft :: BlsqState
builtinRotateStackLeft = do
 st <- getStack
 case st of
   [] -> return ()
   [a] -> return ()
   xs -> putStack $ tail xs ++ [head xs]

-- | #R
builtinRotateStackRight :: BlsqState
builtinRotateStackRight = do
 st <- getStack
 case st of
   [] -> return ()
   [a] -> return ()
   xs -> putStack $ last xs : init xs

-- | cl
builtinCeiling :: BlsqState
builtinCeiling = builtinProduct >> builtinProduct

-- | fo
builtinFloor :: BlsqState
builtinFloor = builtinAverage >> builtinProduct

-- | z?
builtinZero :: BlsqState
builtinZero = do
 st <- getStack
 putResult $
  case st of
   (BlsqInt 0 : xs) -> (BlsqInt 1) : xs
   (BlsqDouble 0.0 : xs) -> (BlsqInt 1) : xs
   (BlsqStr "" : xs) -> (BlsqInt 1) : xs
   (BlsqBlock [] : xs) -> (BlsqInt 1) : xs
   (BlsqChar '\0' : xs) -> (BlsqInt 1) : xs
   (_ : xs) -> (BlsqInt 0) : xs
   _ -> BlsqError "Burlesque: (z?) Stack size error or invalid arguments!" : st

-- | nz
builtinNotZero :: BlsqState
builtinNotZero = builtinZero >> builtinNot

-- | dg
builtinDigits :: BlsqState
builtinDigits = do
 st <- getStack
 putResult $
  case st of
    (BlsqInt b : BlsqInt n : xs) -> (BlsqBlock (map (BlsqInt) (digits b n))) : xs 
    _ -> BlsqError "Burlesque: (dg) Invalid arguments!" : st
    
-- | ug
builtinUnDigits :: BlsqState
builtinUnDigits = do
 st <- getStack
 putResult $
  case st of
    (BlsqInt b : BlsqBlock ns : xs) -> (BlsqInt . unDigits b $
                                       map (\c -> case c of
                                                   BlsqInt q -> q
                                                   _ -> 0) ns) : xs
    _ -> BlsqError "Burlesque: (ug) Invalid arguments!" : st
    
-- | P_
builtinPopFromState2 :: BlsqState
builtinPopFromState2 = do
  s <- popStateStack
  return ()
    
-- | PP
builtinPopFromState :: BlsqState
builtinPopFromState = do
  s <- popStateStack
  pushToStack s
  
-- | Pp
builtinPushToState :: BlsqState
builtinPushToState = do
 st <- getStack
 case st of
   (a : xs) -> pushStateStack a >> putStack xs
   _ -> pushToStack $ BlsqError "Burlesque: (Pp) Invalid stack!"
   
-- | pP
builtinPeekFromState :: BlsqState
builtinPeekFromState = do
  s <- popStateStack
  pushStateStack s
  pushToStack s
  
-- | p/
builtinSwapOnState :: BlsqState
builtinSwapOnState = do
  swapStateStack
  
-- | p\
builtinSwapStacks :: BlsqState
builtinSwapStacks = do
  (st, st', v) <- get
  put (st', st, v)