module Parser.Base 
    ( Parser
    , ParseError
    , nonTerminal
    , terminal
    , pSymbol
    , parseWith
    , blank
    , sc
    , hsc
    , symbol
    , symbolNL
    , parens
    , brackets
    , curly
    , identifier
    , litTerminal
    ) where

import Syntax.Base (NonTerminal(..), Terminal(..), Symbol)
import Data.Void (Void)
import Text.Megaparsec
    ((<|>), empty, Parsec, between, many, (<?>), parse, someTill, ParseErrorBundle)
import Text.Megaparsec.Char (space1, hspace1, letterChar, alphaNumChar, char, eol)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

type Parser = Parsec Void String
type ParseError = ParseErrorBundle String Void

-------------------------------------------------------------------------------
--- Helpers

blank :: Parser ()
blank = Lexer.space space1 empty empty

sc :: Parser ()
sc = Lexer.space space1 lineComment empty

hsc :: Parser ()
hsc = Lexer.space hspace1 lineComment empty

lineComment :: Parser ()
lineComment = void (Lexer.skipLineComment "--") <* eol

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme hsc

symbol :: String -> Parser String
symbol = Lexer.symbol hsc

symbolNL :: String -> Parser String
symbolNL = Lexer.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

-- Não pode ser string vazia
litTerminal :: Parser String
litTerminal = lexeme (char '"' >> someTill Lexer.charLiteral (char '"') <?> "string")
            -- <|> lexeme (char '\'' >> someTill Lexer.charLiteral (char '\'') <?> "string")

-------------------------------------------------------------------------------
--- Base parser

nonTerminal :: Parser NonTerminal
nonTerminal =
    NT <$> identifier <?> "nonTerminal"

terminal :: Parser Terminal
terminal = T <$> litTerminal <?> "terminal"

pSymbol :: Parser Symbol
pSymbol = Left <$> nonTerminal <|> Right <$> terminal

parseWith :: Parser a -> String -> Either ParseError a
parseWith p = parse p ""
