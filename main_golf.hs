import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment
import System.IO
import Data.List

runProgram :: String -> String -> String
runProgram p stdin =
 unlines . map toDisplay . filter notHidden . fst $ execState (eval (runParserWithString parseBlsq p)) ([BlsqStr stdin],[BlsqStr stdin])


main = do
 args <- getArgs
 case args of
   ["--file",file] -> do 
     prog <- readFile file
     interact $ runProgram prog
   _ -> do putStrLn $ "Invalid usage"
           putStrLn "  --file <path>           Read code from file (incl. STDIN)"
           putStrLn ""
           putStrLn "\tBurlesque\tRoman Muentener, 2012"
