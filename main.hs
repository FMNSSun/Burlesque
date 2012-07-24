import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment
import System.IO
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import Data.List

runProgram :: String -> String -> String
runProgram p stdin =
 unlines . map toDisplay $ execState (eval (runParserWithString parseBlsq p)) [BlsqStr stdin]

runProgramNoStdin :: String -> String
runProgramNoStdin p =
 unlines . map toDisplay $ execState (eval (runParserWithString parseBlsq p)) []

printHTML p = putStrLn $ intercalate " " $ map toHTML $ runParserWithString parseBlsq p

main = do
 args <- getArgs
 case args of
   ["--file",file] -> do 
     prog <- readFile file
     interact $ runProgram prog
   ["--file-no-stdin",file] -> do 
     prog <- readFile file
     putStr $ runProgramNoStdin prog
   ["--no-stdin",prog] -> putStr $ runProgramNoStdin prog
   ["--shell"] -> runInputT settings burlesqueShell
   [prog] -> interact $ runProgram prog
   _ -> error "Invalid usage"
 where settings :: Settings IO
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
   Just q -> do outputStrLn $ runProgramNoStdin q
                burlesqueShell
