import Network.CGI
import Text.XHtml.Strict

import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

main = runCGI $ handleErrors cgiMain

page q = do
 output . renderHtml $ body << ((form ! [method "get",action "burlesque.cgi"])
     << (   (input ! [thetype "text", name "q"])
        +++ (input ! [thetype "submit", value "Go!"])
        )) +++ (h1 << "Result") +++ (pre << q)

cgiMain = do
  mn <- getInput "q"
  case mn of
    Nothing -> page ""
    Just q -> page (runProgramNoStdin q)

runProgramNoStdin :: String -> String
runProgramNoStdin p =
 unlines . map toDisplay $ execState (eval (runParserWithString parseBlsq p)) []
