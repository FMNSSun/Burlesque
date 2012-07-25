import Burlesque.Types
import Burlesque.Eval
import Burlesque.Display
program = [BlsqIdent "ri",BlsqBlock [BlsqIdent "^^",BlsqIdent "\\/",BlsqInt 1,BlsqIdent ".-"],BlsqBlock [BlsqInt 1,BlsqIdent ".-"],BlsqIdent "w!",BlsqIdent "vv"]
runProgram stdin = 
  unlines . map toDisplay $ execState (eval program) [BlsqStr stdin]
main = interact $ runProgram
