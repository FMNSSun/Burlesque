import Z.Parser
import Z.Types
import Z.Eval
import Z.Display

import System.Environment
import System.IO
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import Data.List
import System.Timeout
import Control.Exception

--runProgram :: String -> String -> String
--runProgram p stdin =
-- unlines . map toDisplay $ execState (eval (runParserWithString parseZ p)) [ZStr stdin]

runProgramNoStdin :: String -> IO String
runProgramNoStdin p = do
 st <- execStateT (eval (runParserWithString parseZ p)) []
 return . unlines . map toDisplay $ st
 
printHTML p = putStrLn $ intercalate " " $ map toHTML $ runParserWithString parseZ p

main = do
 args <- getArgs
 case args of

   ["--shell"] -> runInputT settings burlesqueShell
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
   Just q -> do s <- liftIO $ runProgramNoStdin q
                outputStr s
                burlesqueShell
