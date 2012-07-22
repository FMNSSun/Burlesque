import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment

runProgram :: String -> String -> String
runProgram p stdin =
 unlines . map toDisplay $ execState (eval (runParserWithString parseBlsq p)) [BlsqStr stdin]

main = do
 args <- getArgs
 case args of
   [file] -> do prog <- readFile file
                interact $ runProgram prog
   _ -> error "Invalid usage"
