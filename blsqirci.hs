import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import System.Environment
import System.IO
import Data.List
import Data.Time
import System.Timeout
import Control.Exception
import Control.DeepSeq

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
   ["--ircbot",prog] -> do result <- timeout 100 $ evaluate $!! runProgramNoStdin prog
                           case result of
                              Nothing -> putStrLn "Ain't nobody got time fo' dat!"
                              Just q -> case lines q of
                                          [] -> putStr "No output!\n"
                                          x:_ -> putStr . take 80 $ x
   _ -> do putStrLn $ "Invalid usage"
           putStrLn "\tBurlesque\tRoman Muentener, 2012"