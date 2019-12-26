{-# LANGUAGE FlexibleContexts #-}

module Burlesque.Parser
  (parseNumber,
   parseDouble,
   parseChar,
   parseIdent,
   parseBlsq,
   parseBlock,
   parseArray,
   parseData,
   parseChar',
   parseQuoted,
   parseSingle,
   runParserWithString,
   runParserWithString')
 where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import Burlesque.Types
import Burlesque.Helpers
import Debug.Trace
import qualified Data.Map as M


-- Usable singles: KO

parseLilO = do
 char 'o'
 times <- many1 digit
 e <- parseSingle
 return $ BlsqAutoBlock (replicate (read times) e)

parseDouble :: Parser BlsqExp
parseDouble = do 
  s <- many $ char '-'
  n1 <- many1 digit
  char '.'
  n2 <- many1 digit
  optional spaces
  if not.null $ s then 
    return $ BlsqDouble (-1 * (read (n1 ++ "." ++ n2)))
  else return $ BlsqDouble (read (n1 ++ "." ++ n2))

parseNumber :: Parser BlsqExp
parseNumber = do 
  s <- many $ char '-'
  num <- many1 digit
  optional spaces
  if not.null $ s then
    return $ BlsqInt (-1 * (read num))
  else return $ BlsqInt (read num)
  
parseIntE :: Parser BlsqExp
parseIntE = do
  s <- many $ char '-'
  num <- many1 digit
  char 'e'
  num2 <- many1 digit
  optional spaces
  if not.null $ s then
    return $ BlsqInt (-1 * ( (read num) * 10^(read num2) ))
  else return $ BlsqInt (1 * ( (read num) * 10^(read num2) ))

parseChar :: Parser BlsqExp
parseChar = do 
  char '\''
  c <- anyChar
  optional spaces
  return $ BlsqChar c

parseChar' :: Parser BlsqExp
parseChar' = do 
  char '\''
  c <- noneOf "'"
  char '\''
  optional spaces
  return $ BlsqChar c
  
parseSingleIdent :: Parser BlsqExp
parseSingleIdent = do
  a <- oneOf "jJQ"
  optional spaces
  return . BlsqIdent $ [a]

parseIdent = (try parseIdent'') <|> parseIdent'
  
parseIdent' :: Parser BlsqExp
parseIdent' = do 
  a <- noneOf "1234567890{}',\" ()yYV"
  b <- anyChar
  optional spaces
  return . BlsqIdent $ a:b:[]
  
parseIdent'' = do
  string "``"
  s <- parseIdent'''
  optional spaces
  return s
  
parseIdent''' = do
  s <- many $ noneOf " \r\n\t"
  optional spaces
  return . BlsqIdent $ s

parseSpecial :: Parser BlsqExp
parseSpecial = do
  b <- oneOf ",)@:%"
  optional spaces
  return $ BlsqSpecial [b]
  
parseSingleBlock :: Parser BlsqExp
parseSingleBlock = do
  char 'q'
  optional spaces
  e <- parseSingle
  optional spaces
  return $ BlsqBlock [e]

parseBlock :: Parser BlsqExp
parseBlock = do
  s <- char '{'
  optional spaces
  e <- parseBlsq
  t <- char '}'
  optional spaces
  return $ BlsqBlock e
  
parseMapBlock :: Parser BlsqExp
parseMapBlock = do
  string "m{"
  optional spaces
  e <- parseBlsq
  optional spaces
  char '}'
  optional spaces
  return $ BlsqAutoBlock [BlsqBlock e, BlsqIdent "m["]
  
parseFilterBlock :: Parser BlsqExp
parseFilterBlock = do
  string "f{"
  optional spaces
  e <- parseBlsq
  optional spaces
  char '}'
  optional spaces
  return $ BlsqAutoBlock [BlsqBlock e, BlsqIdent "f["]
  
parseReduceBlock :: Parser BlsqExp
parseReduceBlock = do
  string "r{"
  optional spaces
  e <- parseBlsq
  optional spaces
  char '}'
  optional spaces
  return $ BlsqAutoBlock [BlsqBlock e, BlsqIdent "r["]

parseArray :: Parser BlsqExp
parseArray = do char '['
                t <- parseArray'
                return $ BlsqBlock t
  where parseArray' :: Parser [BlsqExp]
        parseArray' = do n <- parseData
                         many $ oneOf ", \t"
                         (do t <- parseArray'
                             return (n : t))
                          <|>
                          do char ']'
                             return [n]

parseString :: Parser BlsqExp
parseString = do 
  _ <- char '"'
  e <- many (noneOf "\"")
  _ <- char '"'
  optional spaces
  return $ BlsqStr (unescape e)

parsePretty :: Parser BlsqExp
parsePretty = do
 _ <- char '`'
 e <- many (noneOf "`")
 _ <- char '`'
 optional spaces
 return $ BlsqPretty (BlsqStr e) BlsqFormatNormal

parseProc :: Parser BlsqExp
parseProc = do
  string "proc"
  optional spaces
  ident <- many1 $ noneOf "@%=!?<>: \t\n\r"
  optional spaces
  prog <- parseBlock
  optional spaces
  return $ BlsqAssign ident (prog) False False
 
parseAssign :: Parser BlsqExp
parseAssign = do 
  char '%'
  name <- many $ noneOf "%=!?<>:"
  char '='
  optional spaces
  s <- parseSingle
  return $ BlsqAssign name s False False
  
parseAssign2 :: Parser BlsqExp
parseAssign2 = do
  char 's'
  d <- oneOf "0123456789"
  optional spaces
  return $ BlsqAssign [d] undefined True False
  
parseAssign3 :: Parser BlsqExp
parseAssign3 = do
  char 'S'
  d <- oneOf "0123456789"
  optional spaces
  return $ BlsqAssign [d] undefined True True


parseSet :: Parser BlsqExp
parseSet = do
  string "set"
  optional spaces
  name <- many1 $ noneOf "@%=!?<>: \t\n\r"
  optional spaces
  string "to"
  optional spaces
  exp <- parseBlock
  optional spaces
  return $ BlsqSet name exp

  
parseGet2 :: Parser BlsqExp
parseGet2 = do
  char 'g'
  d <- oneOf "0123456789"
  optional spaces
  return $ BlsqGet [d]
  
parseMap :: Parser BlsqExp
parseMap = do
  char '%'
  char ':'
  def <- parseSingle
  keyValues <- many1 $ parseKeyValues
  char 'V'
  return $ BlsqMap (M.fromList keyValues) def

parseKeyValues :: Parser (BlsqExp, BlsqExp)
parseKeyValues = do
  optional spaces
  key <- parseSingle
  optional spaces 
  value <- parseSingle
  return $ (key, value)
  
parseCall = parseCall' <|> parseCall''

parseCall'' :: Parser BlsqExp
parseCall'' = do
  string "call"
  optional spaces
  ident <- many1 $ noneOf "@%=!?<>: \t\n\r"
  optional spaces
  return $ BlsqCall ident False
  
parseCall' :: Parser BlsqExp
parseCall' = do
  char '%'
  name <- many1 $ noneOf "%=!?<>:"
  char '!'
  optional spaces
  return $ BlsqCall name False

parseGet = parseGet' <|> parseGet''
  
parseGet'' = do
  string "get"
  optional spaces
  ident <- many1 $ noneOf "@%=!?<>: \t\n\r"
  optional spaces
  return $ BlsqGet ident

parseGet' :: Parser BlsqExp
parseGet' = do
  char '%'
  name <- many1 $ noneOf "%=!?<>:"
  char '?'
  optional spaces
  return $BlsqGet name

parseQuoted :: Parser BlsqExp
parseQuoted = do
  _ <- char '('
  optional spaces
  e <- parseSingle
  optional spaces
  _ <- char ')'
  optional spaces
  return $ BlsqQuoted [e]
  
parseQuoted2 :: Parser BlsqExp
parseQuoted2 = do
  _ <- char 'y'
  optional spaces
  e <- many1 $ parseSingle
  optional spaces
  _ <- char 'Y'
  optional spaces
  return $ BlsqQuoted e


parseData :: Parser BlsqExp
parseData = parseString {- <|> parsePretty <|> parseHackMode -} <|> (try parseDouble) <|> (try parseNumber) <|> parseChar' <|> parseArray

parseData'' = parseString <|> (try parseDouble) <|> parseNumber <|> parseBlock

parseData' = do 
  d <- (try parseFancyCall) <|> parseData <|> parseBlock
  optional spaces
  optional $ char ','
  optional spaces
  return d

parseBlsq :: Parser [BlsqExp]
parseBlsq = many parseSingle

parseBlsqFancy :: Parser [BlsqExp]
parseBlsqFancy = many1 $ parseSingleFancy

parseFancyAssign = do
  ident <- many1 $ noneOf "@%=!?<>:() ,{}[]\\\t\n\r"
  optional spaces
  string ":="
  optional spaces
  exp <- parseSingleFancy
  optional spaces
  return $ BlsqSet ident (BlsqBlock [exp, BlsqIdent "bx"])

parseFancyCall :: Parser BlsqExp
parseFancyCall = do
  ident <- many1 $ noneOf "@%=!?<>:() ,{}[]\\\t\n\r"
  optional spaces
  char '('
  optional spaces
  args <- many $ parseData'
  optional spaces
  char ')'
  optional spaces
  return $ BlsqAutoBlock ( (args) ++ [BlsqCall ident True])

parseFancy :: Parser BlsqExp
parseFancy = do
  string "fancy"
  optional spaces
  block <- parseBlsqFancy
  optional spaces
  string "end"
  optional spaces
  return $ BlsqAutoBlock block
  

parseLispExp :: Parser BlsqExp
parseLispExp = parseSExp <|> parseData''
  
parseSExp :: Parser BlsqExp
parseSExp = do
  char '('
  optional spaces
  i <- parseIdent'''
  optional spaces
  args <- many $ parseLispExp
  optional spaces
  char ')'
  optional spaces
  return $ BlsqSExp i args
  
parseLisp :: Parser BlsqExp
parseLisp = do
  string "begin lisp"
  optional spaces
  sexps <- many1 $ parseSExp
  optional spaces
  string "end lisp"
  optional spaces
  return $ BlsqAutoBlock sexps

parseUnfancy = do
  char '\\'
  single <- parseSingle
  optional spaces
  return single
  
parseFancyDef :: Parser BlsqExp
parseFancyDef = do
  string "def"
  optional spaces
  ident <- many1 $ noneOf "@%=!?<>: \\\t\n\r"
  optional spaces
  char ':'
  optional spaces
  block <- parseBlsqFancy
  optional spaces
  string "end"
  optional spaces
  return $ BlsqAssign ident (BlsqBlock block) False False
  
parseSingleFancy :: Parser BlsqExp
parseSingleFancy = (try parseFancyCall) <|> (try parseFancyAssign) <|> parseFancyDef <|> parseData' <|> parseUnfancy

parseSingle :: Parser BlsqExp
parseSingle = (try parseLilO) <|> (try parseLisp) <|> (try parseFancy) <|> (try parseMap) <|> (try parseSet) <|> (try parseGet2) <|> (try parseGet) <|> (try parseAssign2) <|> (try parseAssign3) <|>
              (try parseCall) <|> (try parseAssign) <|> (try parseProc) <|> (try parseBlocks') <|> parseSingleBlock <|> 
              parseBlock <|> parseString {- <|> parsePretty <|> parseHackMode -} <|> parseSpecial <|> 
              (try parseDouble) <|> (try parseIntE) <|> (try parseNumber) <|>
              parseChar <|> parseQuoted <|> parseQuoted2 <|> (try parseTriplet) <|> parseSingleIdent <|> (try parseShortcuts) <|> parseIdent
			  
parseBlocks' = (try parseMapBlock) <|> (try parseFilterBlock) <|> (try parseReduceBlock)

parseTriplet = do
  char 'k'
  a <- parseSingle
  b <- parseSingle
  c <- parseSingle
  return $ BlsqAutoBlock [BlsqIdent "J", a, BlsqIdent "j", c, b]

parseShortcuts = do
  char '`'
  sc
 where sc = sc_FilterNot <|> sc_MapFilterSame
       sc_FilterNot = do
         char 'F'
         s <- parseSingle
         return $ BlsqAutoBlock [BlsqBlock [s], BlsqIdent "fn"]
       sc_MapFilterSame = do
         char 'M'
    	 s <- parseSingle
    	 return $ BlsqAutoBlock [BlsqBlock[s], BlsqIdent "m[", BlsqBlock[s], BlsqIdent "f["]
   
runParserWithString p input = 
  case parse p "" input of
    Left err -> [BlsqError $ show err]
    Right q -> q

runParserWithString' p input = 
  case parse p "" input of
    Left err -> BlsqError $ show err
    Right q -> q
