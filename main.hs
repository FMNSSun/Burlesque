{-# LANGUAGE FlexibleContexts #-}

import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment
import System.IO
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import Data.List

import qualified Data.Map as M

loadPrelude :: IO String
loadPrelude = readFile "Prelude.blsq"

runProgram :: String -> String -> String -> IO String
runProgram p stdin file = do
 p' <- loadPrelude
 result <- execStateT (eval (runParserWithString parseBlsq (p'++p))) ([BlsqStr stdin],[], M.fromList [(BlsqStr "____FILE", BlsqStr file)])
 return . unlines . map toDisplay . filter notHidden . fst' $ result

runProgramNoStdin :: String -> String -> IO String
runProgramNoStdin p file = do
 p' <- loadPrelude
 result <- execStateT (eval (runParserWithString parseBlsq (p'++p))) ([],[], M.fromList [(BlsqStr "____FILE", BlsqStr file)])
 return . unlines . map toDisplay . filter notHidden . fst' $ result

runTheFreakingShell = runInputT settings burlesqueShell
 
main = do
 args <- getArgs
 case args of
   ["--file",file] -> do 
     prog <- readFile file
     cin <- getContents
     cout <- runProgram prog cin file
     putStr cout
   ["--file-no-stdin",file] -> do 
     prog <- readFile file
     cout <- runProgramNoStdin prog file
     putStr cout
   ["--no-stdin",prog] -> do
     cout <- runProgramNoStdin prog ""
     putStr cout
   ["--shell"] -> runInputT settings burlesqueShell
   ["--version"] -> putStrLn "burlesque v1.6.9!"
   ["--stdin",prog] -> do
     cin <- getContents
     p' <- loadPrelude
     cout <- runProgram (p'++prog) cin ""
     putStr cout
   _ -> do putStrLn $ "Invalid usage"
           putStrLn "  --file <path>           Read code from file (incl. STDIN)"
           putStrLn "  --file-no-stdin <path>  Read code from file (excl. STDIN)"
           putStrLn "  --no-stdin <code>       Read code from argv (excl. STDIN)"
           putStrLn "  --shell                 Start in shell mode"
           putStrLn "  --version               Print version info"
           putStrLn "  --compile <path>        Pseudo-compile file to haskell code"
           putStrLn "  --stdin <code>          Read code from argv (incl. STDIN)"
           putStrLn ""
           putStrLn "\tBurlesque\tRoman Muentener, 2012"
settings :: Settings IO
settings = Settings { 
           complete = completeWord Nothing " \t" $ return . search,
           historyFile = Nothing,
           autoAddHistory = True
          }
search s = map simpleCompletion . filter (s `isPrefixOf`) $ map fst builtins
 


burlesqueShell = do
 line <- getInputLine "blsq ) "
 case line of 
   Nothing     -> outputStrLn "* Abort..." >> return ()
   Just "exit!" -> outputStrLn "* Exit!" >> return()
   Just q -> do cout <- lift $ runProgramNoStdin q ""
                outputStr cout
                burlesqueShell
