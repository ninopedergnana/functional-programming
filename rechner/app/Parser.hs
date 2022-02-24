module Parser where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Syntax

type Parser = Parsec Void String

-- | Parsing basics
sc :: Parser () -- space consumer
sc = L.space space lineCmnt empty
  where
    lineCmnt = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

eisP :: Parser Zahl
eisP = do
        symbol "eis"
        Eis