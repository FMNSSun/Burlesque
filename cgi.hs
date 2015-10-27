import Network.CGI
import Text.XHtml.Strict

import Burlesque.Parser
import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display

import qualified Data.Map as M

main = runCGI $ handleErrors cgiMain

linkCSS
 = thelink ! [rel "stylesheet", thetype "text/css", href "./style.css"] << noHtml

documentHtml content
 = (header << ((thetitle << "Burlesque Shell") +++ linkCSS)) +++ (body << content)

page q p = do
 output . renderHtml $ documentHtml $ (thediv ! [theclass "box"]) << (((form ! [method "get",action "burlesque.cgi"])
     << (   (input ! [thetype "text", name "q", value p])
        +++ (input ! [thetype "submit", value "Go!"])
        )) +++ (h1 << "Result") +++ (pre << (primHtml q)))

cgiMain = do
  mn <- getInput "q"
  case mn of
    Nothing -> page "" "Enter your code here!"
    Just q -> page (runProgramNoStdin q) q

runProgramNoStdin :: String -> String
runProgramNoStdin p =
 unlines . map toHTML . filter notHidden . fst' $ execState (eval (runParserWithString parseBlsq p)) ([],[], M.fromList [])
