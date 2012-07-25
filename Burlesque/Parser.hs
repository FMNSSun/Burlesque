module Burlesque.Parser
  (parseNumber,
   parseDouble,
   parseChar,
   parseIdent,
   parseBlsq,
   parseBlock,
   runParserWithString)
 where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import Burlesque.Types
import Debug.Trace

parseDouble :: Parser BlsqExp
parseDouble = do n1 <- many1 digit
                 char '.'
                 n2 <- many1 digit
                 optional spaces
                 return $ BlsqDouble (read (n1 ++ "." ++ n2))

parseNumber :: Parser BlsqExp
parseNumber = do s <- many $ char '-'
                 num <- many1 digit
                 optional spaces
                 if not.null $ s then
                   return $ BlsqInt (-1 * (read num))
                 else return $ BlsqInt (read num)

parseChar :: Parser BlsqExp
parseChar = do char '\''
               c <- anyChar
               optional spaces
               return $ BlsqChar c

parseIdent :: Parser BlsqExp
parseIdent = do a <- noneOf "1234567890{}',\" "
                b <- noneOf "1234567890{}',\" "
                optional spaces
                return . BlsqIdent $ a:b:[]

parseSep :: Parser BlsqExp
parseSep = do b <- char ','
              optional spaces
              return $ BlsqSpecial [b]

parseBlock :: Parser BlsqExp
parseBlock = do s <- char '{'
                e <- parseBlsq
                t <- char '}'
                optional spaces
                return $ BlsqBlock e

parseString :: Parser BlsqExp
parseString = do s <- char '"'
                 e <- many (noneOf "\"")
                 t <- char '"'
                 optional spaces
                 return $ BlsqStr e

parseBlsq :: Parser [BlsqExp]
parseBlsq = many parseBlsq'
 where parseBlsq' = parseBlock <|> parseString <|> parseSep <|> (try parseDouble) <|> (try parseNumber) <|> parseChar <|> parseIdent

runParserWithString p input = 
  case parse p "" input of
    Left err -> [BlsqError $ show err]
    Right q -> q
