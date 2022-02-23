module Parser where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Parsec Basics 

charC :: Char -> Parser ()
charC c = do
  _ <- char c
  return ()

sc :: Parser () -- space consumer
sc = L.space space1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

int :: Parser Int
int = fromInteger <$> integer

semi :: Parser String
semi = symbol ";"