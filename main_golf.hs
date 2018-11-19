{-# LANGUAGE FlexibleContexts #-}

import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment
import System.IO
import Data.List

import qualified Data.Map as M

runProgram :: String -> String -> IO String
runProgram p stdin = do
 result <- execStateT (eval (runParserWithString parseBlsq (p))) ([BlsqStr stdin],[], M.fromList [])
 return . unlines . map toDisplay . filter notHidden . fst' $ result

main = do
 args <- getArgs
 case args of
   ["--file",file] -> do 
     prog <- readFile file
     cin <- getContents
     cout <- runProgram prog cin
     putStr cout
   _ -> do putStrLn $ "Invalid usage"
           putStrLn "  --file <path>           Read code from file (incl. STDIN)"
           putStrLn ""
           putStrLn "\tBurlesque\tRoman Muentener, 2012"
