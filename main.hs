import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment
import System.IO
import System.Console.Readline

runProgram :: String -> String -> String
runProgram p stdin =
 unlines . map toDisplay $ execState (eval (runParserWithString parseBlsq p)) [BlsqStr stdin]

runProgramNoStdin :: String -> String
runProgramNoStdin p =
 unlines . map toDisplay $ execState (eval (runParserWithString parseBlsq p)) []

main = do
 args <- getArgs
 case args of
   ["--file",file] -> do prog <- readFile file
                         interact $ runProgram prog
   ["--file-no-stdin",file] -> do prog <- readFile file
                                  putStr $ runProgramNoStdin prog
   ["--no-stdin",prog] -> putStr $ runProgramNoStdin prog
   ["--shell"] -> do hSetBuffering stdin LineBuffering
                     hSetBuffering stdout NoBuffering
                     burlesqueShell
   [prog] -> interact $ runProgram prog
   _ -> error "Invalid usage"


burlesqueShell = do
 line <- readline "blsq ) "
 case line of 
   Nothing     -> putStrLn "* Abort..."
   Just "exit!" -> putStrLn "* Exit!"
   Just q -> do addHistory q
                putStrLn $ runProgramNoStdin q
                burlesqueShell
