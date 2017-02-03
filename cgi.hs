{-# LANGUAGE FlexibleContexts #-}

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
     << (   ((textarea ! [name "q", rows "25", cols "80"]) << p)
        +++ br +++ (input ! [thetype "submit", value "Go!"])
        )) +++ (h1 << "Result") +++ (pre << (primHtml q)))

cgiMain = do
  mn <- getInput "q"
  case mn of
    Nothing -> page "" "Enter your code here!"
    Just q -> do result <- liftIO $ runProgramNoStdin q
                 page result q

runProgramNoStdin :: String -> IO String
runProgramNoStdin p = do
  result <- execStateT (eval (runParserWithString parseBlsq p)) ([],[], M.fromList [])
  return . unlines . map toHTML . filter notHidden . fst' $ result
