module Parser where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Syntax (Zahl, Operator (Plus), Eingabe, RechnungsElemente)

type Parser = Parsec Void String

-- Parsec Basics 

sc :: Parser () -- space consumer
sc = L.space space1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


-- Rechner Parser

eisP :: Parser Zahl
eisP = do
  symbol "eis"
  return Eis

zweiP :: Parser Zahl
zweiP = do
  symbol "zwei"
  return Zwei

plusP :: Parser Operator
plusP = do 
    symbol "plus"
    return Plus

gleichP :: Parser Operator
gleichP = do
	symbol "gleich"
	return Gleich

rechnungsElementeP :: Parser RechnungsElemente
rechnungsElementeP = 
	try eisP 
	<|> zweiP
	<|> plusP 


-- | Eingabe parser
eingabeP :: Parser [RechnungsElemente]
eingabeP = sepBy1 symbol (symbol " ")