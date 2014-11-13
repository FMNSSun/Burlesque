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

runProgram :: String -> String -> String
runProgram p stdin =
 unlines . map toDisplay . filter notHidden . fst' $ execState (eval (runParserWithString parseBlsq p)) ([BlsqStr stdin],[], M.fromList [])

runProgramNoStdin :: String -> String
runProgramNoStdin p =
 unlines . map toDisplay . filter notHidden . fst' $ execState (eval (runParserWithString parseBlsq p)) ([],[], M.fromList [])

runTheFreakingShell = runInputT settings burlesqueShell
 
main = do
 args <- getArgs
 case args of
   ["--file",file] -> do 
     prog <- readFile file
     interact $ runProgram prog
   ["--compile",file] -> do
     prog <- readFile file
     putStrLn "import Burlesque.Types"
     putStrLn "import Burlesque.Eval"
     putStrLn "import Burlesque.Display"
     putStrLn $ "program = " ++ (show $ runParserWithString parseBlsq prog)
     putStrLn $ "runProgram stdin = "
     putStrLn $ "  unlines . map toDisplay $ execState (eval program) [BlsqStr stdin]"
     putStrLn $ "main = interact $ runProgram"
   ["--file-no-stdin",file] -> do 
     prog <- readFile file
     putStr $ runProgramNoStdin prog
   ["--no-stdin",prog] -> putStr $ runProgramNoStdin prog
   ["--shell"] -> runInputT settings burlesqueShell
   ["--version"] -> putStrLn "burlesque v1.6.9!"
   ["--stdin",prog] -> interact $ runProgram prog
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
   Just q -> do outputStr $ runProgramNoStdin q
                burlesqueShell
