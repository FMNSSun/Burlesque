module Burlesque.Display
  (toDisplay, toHTML)
 where

import Burlesque.Types

import Data.List


toDisplay (BlsqInt i) = show i
toDisplay (BlsqBlock xs) = "{" ++ (intercalate " " $ map toDisplay xs) ++ "}"
toDisplay (BlsqStr s) = show s
toDisplay (BlsqError s) = "ERROR: " ++ s
toDisplay (BlsqIdent s) = s
toDisplay (BlsqChar c) = "'"++[c]
toDisplay (BlsqDouble d) = show d
toDisplay q = show q

toHTML a@(BlsqInt _) = "<span class=\"int\">" ++ toDisplay a ++ "</span>"
toHTML a@(BlsqBlock xs) = "<span class=\"blck\">{</span>" ++ (intercalate " " $ map toHTML xs) ++ "<span class=\"blck\">}</span>"
toHTML a@(BlsqStr _) = "<span class=\"str\">" ++ toDisplay a ++ "</span>"
toHTML a@(BlsqError _) = "<span class=\"err\">" ++ toDisplay a ++ "</span>"
toHTML a@(BlsqIdent _) = "<span class=\"id\">" ++ toDisplay a ++ "</span>"
toHTML a@(BlsqChar _) = "<span class=\"chr\">" ++ toDisplay a ++ "</span>"
toHTML a@(BlsqDouble _) = "<span class=\"dbl\">" ++ toDisplay a ++ "</span>"
