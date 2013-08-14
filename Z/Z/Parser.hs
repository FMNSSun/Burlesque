module Z.Parser
  (parseNumber,
   parseDouble,
   parseChar,
   parseIdent,
   parseZ,
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

import Z.Types
import Z.Helpers
import Debug.Trace

parseDouble :: Parser ZExp
parseDouble = do 
  s <- many $ char '-'
  n1 <- many1 digit
  char '.'
  n2 <- many1 digit
  optional spaces
  if not.null $ s then 
    return $ ZDouble (-1 * (read (n1 ++ "." ++ n2)))
  else return $ ZDouble (read (n1 ++ "." ++ n2))

parseNumber :: Parser ZExp
parseNumber = do 
  s <- many $ char '-'
  num <- many1 digit
  optional spaces
  if not.null $ s then
    return $ ZInt (-1 * (read num))
  else return $ ZInt (read num)

parseChar :: Parser ZExp
parseChar = do 
  char '\''
  c <- anyChar
  optional spaces
  return $ ZChar c

parseChar' :: Parser ZExp
parseChar' = do 
  char '\''
  c <- noneOf "'"
  char '\''
  optional spaces
  return $ ZChar c

parseIdent :: Parser ZExp
parseIdent = do 
  a <- noneOf "1234567890{}',\" "
  b <- noneOf "1234567890{}',\" "
  optional spaces
  return . ZIdent $ a:b:[]

parseSep :: Parser ZExp
parseSep = do
  b <- oneOf ",)@:"
  optional spaces
  return $ ZSpecial [b]

parseBlock :: Parser ZExp
parseBlock = do
  s <- char '{'
  optional spaces
  e <- parseZ
  t <- char '}'
  optional spaces
  return $ ZBlock e

parseArray :: Parser ZExp
parseArray = do char '['
                t <- parseArray'
                return $ ZBlock t
  where parseArray' :: Parser [ZExp]
        parseArray' = do n <- parseData
                         many $ oneOf ", \t"
                         (do t <- parseArray'
                             return (n : t))
                          <|>
                          do char ']'
                             return [n]

parseString :: Parser ZExp
parseString = do 
  _ <- char '"'
  e <- many (noneOf "\"")
  _ <- char '"'
  optional spaces
  return $ ZStr (unescape e)

parseQuoted :: Parser ZExp
parseQuoted = do
  _ <- char '('
  optional spaces
  e <- parseSingle
  optional spaces
  _ <- char ')'
  optional spaces
  return $ ZQuoted e

parseData :: Parser ZExp
parseData = parseString <|> (try parseDouble) <|> (try parseNumber) <|> parseChar' <|> parseArray

parseZ :: Parser [ZExp]
parseZ = many parseSingle

parseSingle :: Parser ZExp
parseSingle = parseBlock <|> parseString <|> parseSep <|> 
              (try parseDouble) <|> (try parseNumber) <|>
              parseChar <|> parseQuoted <|> parseIdent

runParserWithString p input = 
  case parse p "" input of
    Left err -> [ZError $ show err]
    Right q -> q

runParserWithString' p input = 
  case parse p "" input of
    Left err -> ZError $ show err
    Right q -> q
