import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment
import System.IO
import Data.List
import System.Timeout
import Control.Exception

runProgram :: String -> String -> String
runProgram p stdin =
 unlines . map toDisplay . filter notHidden $ execState (eval (runParserWithString parseBlsq p)) [BlsqStr stdin]

runProgramNoStdin :: String -> String
runProgramNoStdin p =
 unlines . map toDisplay . filter notHidden $ execState (eval (runParserWithString parseBlsq p)) []

printHTML p = putStrLn $ intercalate " " $ map toHTML $ runParserWithString parseBlsq p

main = do
 args <- getArgs
 case args of
   ["--ircbot",prog] -> do timeout (100) (do result <- evaluate $ runProgramNoStdin prog
                                             putStr result)
                           return ()
   _ -> do putStrLn $ "Invalid usage"
           putStrLn "\tBurlesque\tRoman Muentener, 2012"