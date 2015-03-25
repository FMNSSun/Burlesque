module Burlesque.Helpers
  (replace,
   noSpaces,
   unescape,
   insertAt,
   removeAt,
   setAt,
   toBase,
   fromBase,
   pfactor,
   genSubstrings)
 where

import Data.List
import Data.List.Split
import Data.Digits
import Data.Maybe
import Data.Ord

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

baseStr = ['0'..'9'] ++ ['a'..'z'] ++ ['!'..'/'] ++ [':'..'@'] ++ ['['..'`'] ++ ['{'..'~'] ++ ['A'..'Z']

toBase :: Integral n => n -> n -> String
toBase bs 0 = "0"
toBase bs n = map (\c -> baseStr !! (fromIntegral c)) $ digits bs n


fromBase bs n = unDigits bs $ map (fromIntegral.(\c -> fromMaybe 0 $ findIndex (==c) baseStr)) n

pfactor 1 = []
pfactor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ pfactor $ div n prime
              
substrings s = tail . sort . nub . concatMap tails $ inits s

sortedSubstrings xs = sortBy (comparing length) xs

genSubstrings s = sortedSubstrings . substrings $ s

