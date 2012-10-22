module Burlesque.Helpers
  (replace,
   noSpaces,
   unescape)
 where

import Data.List
import Data.List.Split

replace o n xs = intercalate n . splitOn o $ xs

dropElem _ [] = []
dropElem q (x:xs) 
 |x==q = dropElem q xs
 |otherwise = x : dropElem q xs

--noSpaces xs = replace " " "" xs
noSpaces = dropElem ' '

unescape ('\\':'\\':xs) = '\\' : unescape xs
unescape ('\\':'n':xs) = '\n' : unescape xs
unescape ('\\':'t':xs) = '\t' : unescape xs
unescape ('\\':'r':xs) = '\r' : unescape xs
unescape ('\\':'\'':xs) = '\"' : unescape xs
unescape (x:xs) = x : unescape xs
unescape [] = []

insertAt n e xs = let (a,b) = splitAt n xs in (a ++ [e]) ++ b

removeAt n xs = let (a,b) = splitAt n xs in a ++ (tail b)

setAt n e xs = let (a,b) = splitAt n xs in a ++ (e : tail b)
