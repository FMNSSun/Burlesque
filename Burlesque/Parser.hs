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

parseIdent :: Parser BlsqExp
parseIdent = do 
  a <- noneOf "1234567890{}',\" "
  b <- noneOf "1234567890{}',\" "
  optional spaces
  return . BlsqIdent $ a:b:[]

parseSep :: Parser BlsqExp
parseSep = do
  b <- oneOf ",)@:%"
  optional spaces
  return $ BlsqSpecial [b]

parseBlock :: Parser BlsqExp
parseBlock = do
  s <- char '{'
  optional spaces
  e <- parseBlsq
  t <- char '}'
  optional spaces
  return $ BlsqBlock e

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
  return $ BlsqQuoted e

parseData :: Parser BlsqExp
parseData = parseString {- <|> parsePretty <|> parseHackMode -} <|> (try parseDouble) <|> (try parseNumber) <|> parseChar' <|> parseArray

parseBlsq :: Parser [BlsqExp]
parseBlsq = many parseSingle

parseSingle :: Parser BlsqExp
parseSingle = parseBlock <|> parseString {- <|> parsePretty <|> parseHackMode -} <|> parseSep <|> 
              (try parseDouble) <|> (try parseNumber) <|>
              parseChar <|> parseQuoted <|> parseIdent

runParserWithString p input = 
  case parse p "" input of
    Left err -> [BlsqError $ show err]
    Right q -> q

runParserWithString' p input = 
  case parse p "" input of
    Left err -> BlsqError $ show err
    Right q -> q
