import Debug.Trace

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import System.Environment
import System.IO
import Data.List
import Data.Time
import System.Timeout
import Control.Exception hiding (try)
import Control.DeepSeq

data T = Bin String [T] | List [T] | Value Int | Get Int | Recurse [T] | If String T T T T | Uni String T | Lazy T
 deriving (Eq,Show,Ord)
 
parseT :: Parser T
parseT = do
 (try parseBin) <|> (try parseValue) <|> (try parseRecurse) <|> (try parseGet) <|> (try parseIf) <|> (try parseUni) <|> (try parseList)
 
 
parseGet :: Parser T
parseGet = do
 char '$'
 i <- parseInteger
 optional spaces
 return $ Get i
 
parseRecurse :: Parser T
parseRecurse = do
 char '('
 optional spaces
 char 'r'
 optional spaces
 ts <- many1 parseT
 optional spaces
 char ')'
 return $ Recurse ts
 
parseValue :: Parser T
parseValue = do
 i <- parseInteger
 optional spaces
 return $ Value i
 
parseInteger :: Parser Int
parseInteger = do 
  s <- many $ char '-'
  num <- many1 digit
  optional spaces
  if not.null $ s then
    return $ (-1 * (read num))
  else return $ (read num)
 
parseBin :: Parser T
parseBin = do
 char '('
 optional spaces
 cmd <- string "add" <|> string "mul" <|> string "div" <|> string "sub" <|> string "cons" <|> string "get"
 optional spaces
 ts <- many1 parseT
 optional spaces
 char ')'
 optional spaces
 return $ Bin cmd ts
 
parseList :: Parser T
parseList = do
 char '['
 optional spaces
 cm <- many1 parseT
 optional spaces
 char ']'
 optional spaces
 return $ List cm
 
parseLazy :: Parser T
parseLazy = do
  char '{'
  optional spaces
  cm <- parseT
  optional spaces
  char '}'
  return $ Lazy cm
   
parseIf :: Parser T
parseIf = do
 char '('
 optional spaces
 cmd <- string "if==" <|> string "if<"
 optional spaces
 a <- parseT
 optional spaces
 b <- parseT
 optional spaces
 c <- parseT
 optional spaces
 d <- parseT
 optional spaces
 char ')'
 return $ If cmd a b c d
 
parseUni :: Parser T
parseUni = do
 char '('
 optional spaces
 cmd <- string "odd" <|> string "head" <|> string "tail" <|> string "null" <|> string "$"
 optional spaces
 t <- parseT
 optional spaces
 char ')'
 return $ Uni cmd t
 
runParserWithString p input = 
  case parse p "" input of
    Left err -> error $ show err
    Right q -> q
 
eval' c args = eval c c args

eval :: T -> T -> [T] -> T
eval (Bin "add" xs) r args = 
  foldl1 (\a b -> case eval a r args of
                    Value qa -> case eval b r args of
                                  Value qb -> Value (qa + qb)
                                  _ -> Value 0
                    _ -> Value 0) xs
eval (Bin "mul" xs) r args = 
  foldl1 (\a b -> case eval a r args of
                    Value qa -> case eval b r args of
                                  Value qb -> Value (qa * qb)
                                  _ -> Value 0
                    _ -> Value 0) xs
eval (Bin "div" xs) r args = 
  foldl1 (\a b -> case eval a r args of
                    Value qa -> case eval b r args of
                                  Value qb -> Value (qa `div` qb)
                                  _ -> Value 0
                    _ -> Value 0) xs
eval (Bin "sub" xs) r args = 
  foldl1 (\a b -> case eval a r args of
                    Value qa -> case eval b r args of
                                  Value qb -> Value (qa - qb)
                                  _ -> Value 0
                    _ -> Value 0) xs
eval (Bin "cons" (a:(List x):xs)) r args =
  List $ a:x
eval (Bin "get" ((Value a) : (List x) : xs)) r args =
  x !! a
eval (Bin a xs) r args = eval (Bin a (map (\c -> eval c r args) xs)) r args
eval (Uni "head" x) r args = case eval x r args of
                               List qn -> eval (head qn) r args
                               _ -> Value 0
eval (Uni "tail" x) r args = case eval x r args of
                               List qn -> eval (List (tail qn)) r args
                               _ -> Value 0
eval (Uni "null" x) r args = case eval x r args of
                               List [] -> Value 1
                               _ -> Value 0
eval (Get i) r args = (args !! i)
eval (Recurse xs) r args = eval r r ((map (\t -> eval t r args)) xs)
eval (If "if==" p q a b) r args = if (eval p r args) == (eval q r args) then eval a r args else eval b r args
eval (If "if<" p q a b) r args = if (eval p r args) < (eval q r args) then eval a r args else eval b r args
eval (Uni "odd" n) r args = case eval n r args of
                        Value qn -> if odd qn then Value 1 else Value 0
                        _ -> Value 0
eval (List x) r args = List $ map (\c -> eval c r args) x
eval a r args = a



collatz = runParserWithString parseT "(if== $0 1 1 (if== 1 (odd $0) (r (add (mul 3 $0) 1)) (r (div $0 2))))"

main = do
  c <- getArgs
  case c of
    ["--ircbot", prog] -> do result <- timeout 3000 $ evaluate $!! show $ eval' (runParserWithString parseT prog) [Value 0]
                             case result of
                              Nothing -> putStrLn "Ain't nobody got time fo' dat!"
                              Just q -> case lines q of
                                          [] -> putStr "No output!\n"
                                          x:_ -> putStr . take 80 $ x
