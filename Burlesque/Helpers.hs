module Burlesque.Helpers
  (replace,
   noSpaces)
 where

import Data.List
import Data.List.Split

replace o n xs = intercalate n . splitOn o $ xs
--noSpaces xs = replace " " "" xs
dropElem _ [] = []
dropElem q (x:xs) 
 |x==q = dropElem q xs
 |otherwise = x : dropElem q xs
noSpaces = dropElem ' '
