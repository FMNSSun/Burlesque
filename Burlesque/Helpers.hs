module Burlesque.Helpers
  (replace,
   noSpaces,
   unescape,
   insertAt,
   removeAt,
   setAt,
   toBase,
   fromBase,
   pfactor)
 where

import Data.List
import Data.List.Split
import Data.Digits
import Data.Maybe

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

insertAt n e xs = let (a,b) = splitAt (fromIntegral n) xs in (a ++ [e]) ++ b

removeAt n xs = let (a,b) = splitAt (fromIntegral n) xs in a ++ (tail b)

setAt n e xs = let (a,b) = splitAt (fromIntegral n) xs in a ++ (e : tail b)

toBase bs n = map (\c -> (['0'..'9'] ++ ['a'..'z']) !! c) $ digits bs n
fromBase bs n = unDigits bs $ map (\c -> fromMaybe 0 $ findIndex (==c) (['0'..'9'] ++ ['a'..'z'])) n

pfactor 1 = []
pfactor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ pfactor $ div n prime

