module Z.Display
  (toDisplay, toHTML)
 where

import Z.Types
import Z.Helpers

import Data.List
import Web.Encodings
import Numeric

toDisplay (ZInt i) = show i
toDisplay (ZBlock xs) = "{" ++ (intercalate " " $ map toDisplay xs) ++ "}"
toDisplay (ZStr s) = s
toDisplay (ZError s) = "ERROR: " ++ s
toDisplay (ZIdent s) = s
toDisplay (ZChar c) = "'"++[c]
toDisplay (ZDouble d) = showFFloat Nothing (d) ""

toDisplay (ZSpecial q) = q
toDisplay (ZQuoted q) = "(" ++ toDisplay q ++ ")"
toDisplay (ZNil) = "_|_"


toDisplay q = show q

toHTML a@(ZInt _) = "<span class=\"int\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(ZBlock xs) = "<span class=\"blck\">{</span>" ++ (intercalate " " $ map toHTML xs) ++ "<span class=\"blck\">}</span>"
toHTML a@(ZStr _) = "<span class=\"str\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(ZError _) = "<span class=\"err\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(ZIdent _) = "<span class=\"id\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(ZChar _) = "<span class=\"chr\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML a@(ZDouble _) = "<span class=\"dbl\">" ++ (encodeHtml $ toDisplay a) ++ "</span>"
toHTML q = "<span class=\"raw\">" ++ (encodeHtml $ toDisplay q) ++ "</span>"
