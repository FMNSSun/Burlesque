import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval

import System.Environment

runProgram p =
 mapM_ print $ execState (eval (runParserWithString parseBlsq p)) []

main = do
 args <- getArgs
 case args of
   [file] -> do readFile file >>= runProgram
   _ -> error "Invalid usage"
