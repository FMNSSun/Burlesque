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


-- Usable singles: kKoO

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

parseIdent :: Parser BlsqExp
parseIdent = do 
  a <- noneOf "1234567890{}',\" ()yYV"
  b <- anyChar
  optional spaces
  return . BlsqIdent $ a:b:[]

parseSep :: Parser BlsqExp
parseSep = do
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
  return $ BlsqMapBlock e

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
  return $ BlsqCall ident
  
parseCall' :: Parser BlsqExp
parseCall' = do
  char '%'
  name <- many1 $ noneOf "%=!?<>:"
  char '!'
  optional spaces
  return $ BlsqCall name

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
 
parseHackMode :: Parser BlsqExp
parseHackMode = do
 _ <- char '#'
 e <- many (noneOf "#")
 _ <- char '#'
 optional spaces
 return $ BlsqHackMode (unescape e)

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

parseBlsq :: Parser [BlsqExp]
parseBlsq = many parseSingle

parseSingle :: Parser BlsqExp
parseSingle = (try parseMap) <|> (try parseSet) <|> (try parseGet2) <|> (try parseGet) <|> (try parseAssign2) <|> (try parseAssign3) <|>
              (try parseCall) <|> (try parseAssign) <|> (try parseProc) <|> (try parseMapBlock) <|> parseSingleBlock <|> 
              parseBlock <|> parseString {- <|> parsePretty <|> parseHackMode -} <|> parseSep <|> 
              (try parseDouble) <|> (try parseIntE) <|> (try parseNumber) <|>
              parseChar <|> parseQuoted <|> parseQuoted2 <|> parseSingleIdent <|> parseIdent

runParserWithString p input = 
  case parse p "" input of
    Left err -> [BlsqError $ show err]
    Right q -> q

runParserWithString' p input = 
  case parse p "" input of
    Left err -> BlsqError $ show err
    Right q -> q
