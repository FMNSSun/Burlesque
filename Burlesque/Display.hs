module Burlesque.Display
  (toDisplay, toHTML)
 where

import Burlesque.Types
import Burlesque.Helpers

import Data.List
import Web.Encodings


toDisplay (BlsqInt i) = show i
toDisplay (BlsqBlock xs) = "{" ++ (intercalate " " $ map toDisplay xs) ++ "}"
toDisplay (BlsqStr s) = show s
toDisplay (BlsqError s) = "ERROR: " ++ s
toDisplay (BlsqIdent s) = s
toDisplay (BlsqChar c) = "'"++[c]
toDisplay (BlsqDouble d) = show d
toDisplay (BlsqPretty (BlsqStr s) BlsqFormatNormal) = s
toDisplay (BlsqPretty (BlsqStr s) BlsqFormatNoSpaces) = noSpaces s
toDisplay (BlsqPretty (BlsqBlock xs) BlsqFormatNormal) = 
  "[" ++ (intercalate ", " $ map (\c -> toDisplay $ BlsqPretty c BlsqFormatNormal) xs) ++ "]"
toDisplay (BlsqPretty f BlsqFormatNormal) = toDisplay f


toDisplay q = show q

toHTML a@(BlsqInt _) = "<span class=\"int\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqBlock xs) = "<span class=\"blck\">{</span>" ++ (intercalate " " $ map toHTML xs) ++ "<span class=\"blck\">}</span>"
toHTML a@(BlsqStr _) = "<span class=\"str\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqError _) = "<span class=\"err\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqIdent _) = "<span class=\"id\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqChar _) = "<span class=\"chr\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqDouble _) = "<span class=\"dbl\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML q = "<span class=\"raw\">" ++ (encodeHtml $ toDisplay q) ++ "</span>"
