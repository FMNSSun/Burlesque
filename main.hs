import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment

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
   [prog] -> interact $ runProgram prog
   _ -> error "Invalid usage"
