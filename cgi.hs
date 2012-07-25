import Network.CGI
import Text.XHtml.Strict

import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

main = runCGI $ handleErrors cgiMain

linkCSS
 = thelink ! [rel "stylesheet", thetype "text/css", href "style.css"] << noHtml

documentHtml content
 = (header << ((thetitle << "Burlesque Shell") +++ linkCSS)) +++ (body << content)

page q = do
 output . renderHtml $ documentHtml $ ((form ! [method "get",action "burlesque.cgi"])
     << (   (input ! [thetype "text", name "q"])
        +++ (input ! [thetype "submit", value "Go!"])
        )) +++ (h1 << "Result") +++ (pre << (primHtml q))

cgiMain = do
  mn <- getInput "q"
  case mn of
    Nothing -> page ""
    Just q -> page (runProgramNoStdin q)

runProgramNoStdin :: String -> String
runProgramNoStdin p =
 unlines . map toHTML $ execState (eval (runParserWithString parseBlsq p)) []
