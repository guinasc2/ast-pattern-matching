module Parser.Base where

import Syntax.Base
    (NonTerminal(..), Terminal(..), Symbol, Pretty(..))
import Data.Void (Void)
import Text.Megaparsec
    ((<|>), empty, Parsec, between, many, (<?>), manyTill, parse, errorBundlePretty, someTill)
import Text.Megaparsec.Char
    (space1, hspace1, letterChar, alphaNumChar, char, eol)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad

type Parser = Parsec Void String

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

litString :: Parser String
-- someTill pq só é usado para terminal e eu decidi que ele não pode ser vazio
litString = lexeme (char '"' >> someTill Lexer.charLiteral (char '"') <?> "string")
            -- <|> lexeme (char '\'' >> someTill Lexer.charLiteral (char '\'') <?> "string")

-------------------------------------------------------------------------------
--- Base parser

nonTerminal :: Parser NonTerminal
nonTerminal =
    NT <$> identifier <?> "nonTerminal"

terminal :: Parser Terminal
terminal = T <$> litString <?> "terminal"

pSymbol :: Parser Symbol
pSymbol = Left <$> nonTerminal <|> Right <$> terminal

-------------------------------------------------------------------------------
--- Testes

parseFromFile :: Show a => Parser a -> FilePath -> IO ()
parseFromFile p f = do
        contents <- readFile f
        case parse p "" contents of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right xs -> print xs

parseFromFilePretty :: Pretty a => Parser a -> FilePath -> IO ()
parseFromFilePretty p f = do
        contents <- readFile f
        case parse p "" contents of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right xs -> putStr $ show (pPrint xs)