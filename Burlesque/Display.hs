module Burlesque.Display
  (toDisplay, notHidden)
 where

import Burlesque.Types
import Burlesque.Helpers

import Data.List
import Numeric

-- This is the ugliest hack in the history of programming.
-- Nothing to see here: Move along.
-- I SAID MOVE ALONG!
notHidden (BlsqHiddenState _) = False
notHidden _ = True

toDisplay (BlsqHiddenState _) = ""

toDisplay (BlsqInt i) = show i
toDisplay (BlsqBlock xs) = "{" ++ (intercalate " " $ map toDisplay xs) ++ "}"
toDisplay (BlsqStr s) = show s
toDisplay (BlsqError s) = "ERROR: " ++ s
toDisplay (BlsqIdent s) = s
toDisplay (BlsqChar c) = "'"++[c]
toDisplay (BlsqDouble d) = showFFloat Nothing (d) ""

toDisplay (BlsqPretty (BlsqStr s) BlsqFormatNormal) = s
toDisplay (BlsqPretty (BlsqStr s) BlsqFormatNoSpaces) = noSpaces s

toDisplay (BlsqPretty (BlsqDouble a) _) = show a

toDisplay (BlsqPretty (BlsqChar c) BlsqFormatNormal) = [c]

toDisplay (BlsqPretty (BlsqBlock xs) BlsqFormatNormal) = 
  "[" ++ (intercalate ", " $ map prettify' xs) ++ "]"
 where prettify' a@(BlsqStr c) = toDisplay a
       prettify' c = toDisplay $ BlsqPretty c BlsqFormatNormal


toDisplay (BlsqPretty (BlsqBlock xs) BlsqFormatWithSpaces) =
 "[" ++ (intercalate " " $ map prettify' xs) ++ "]"
 where prettify' a@(BlsqStr c) = toDisplay a
       prettify' c = toDisplay $ BlsqPretty c BlsqFormatWithSpaces

toDisplay (BlsqPretty (BlsqBlock xs) BlsqFormatNoSpaces) =
 "[" ++ (intercalate "," $ map prettify' xs) ++ "]"
 where prettify' a@(BlsqStr c) = toDisplay a
       prettify' c = toDisplay $ BlsqPretty c BlsqFormatNoSpaces

toDisplay (BlsqPretty f _) = toDisplay f

toDisplay (BlsqSpecial q) = q
toDisplay (BlsqQuoted q) = "(" ++ toDisplay q ++ ")"
toDisplay (BlsqNil) = "_|_"

toDisplay (BlsqHackMode x) = "#" ++ x ++ "#"

toDisplay q = show q