{-# LANGUAGE FlexibleContexts #-}

module Burlesque.Display
  (toDisplay, notHidden, toHTML)
 where

import Burlesque.Types
import Burlesque.Helpers

import Data.List
import Numeric
import qualified Data.Map as M

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

toDisplay (BlsqPretty (BlsqDouble a) _) = showFFloat Nothing (a) ""

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
toDisplay (BlsqQuoted q) = "(" ++ (intercalate " " $ map toDisplay q) ++ ")"
toDisplay (BlsqNil) = "_|_"

toDisplay (BlsqMap m _) = intercalate "" $  (map (\(a,b) -> "<" ++ toDisplay a ++ "," ++ toDisplay b ++ ">") $ M.toList m)

toDisplay q = "__INTERNAL__:" ++ (show q)

toHTML a@(BlsqInt _) = "<span class=\"int\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqBlock xs) = "<span class=\"blck\">{</span>" ++ (intercalate " " $ map toHTML xs) ++ "<span class=\"blck\">}</span>"
toHTML a@(BlsqStr _) = "<span class=\"str\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqError _) = "<span class=\"err\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqIdent _) = "<span class=\"id\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqChar _) = "<span class=\"chr\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(BlsqDouble _) = "<span class=\"dbl\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML q = "<span class=\"raw\">" ++ (encodeHtml $ toDisplay q) ++ "</span>"

escapeHtml :: Char -> String
escapeHtml '<' = "&lt;"
escapeHtml '>' = "&gt;"
escapeHtml '&' = "&amp;"
escapeHtml '"' = "&quot;"
escapeHtml '\'' = "&#39;"
escapeHtml x = [x]

encodeHtml = concatMap escapeHtml
