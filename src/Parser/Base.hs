{-# LANGUAGE TupleSections #-}

module Parser.Base where

import Syntax.Base
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators.Expr
-- import qualified Text.PrettyPrint.HughesPJ

type Parser = Parsec Void String

sc :: Parser ()
sc = Lexer.space space1 empty empty

hsc :: Parser ()
hsc = Lexer.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: String -> Parser String
symbol = Lexer.symbol sc

hsymbol :: String -> Parser String
hsymbol = Lexer.symbol hsc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

litString :: Parser String
litString = lexeme (char '"' >> manyTill Lexer.charLiteral (char '"') <?> "string")

-------------------------------------------------------------------------------
-- Regex parser

token :: Parser Syntax.Base.Token
token = choice [
            Identifier <$ symbol "identifier",
            Digit <$ symbol "digit",
            Integer <$ symbol "int",
            Double <$ symbol "double",
            SignedInteger <$ symbol "signedInterger",
            SignedDouble <$ symbol "signedDouble",
            Num <$ symbol "num",
            SignedNum <$ symbol "signedNum",
            StringLiteral <$ symbol "string",
            Exact <$> litString,
            RegRef <$> identifier
        ] <?> "token"

regex :: Parser NamedRegex
regex = f <$> identifier <*> symbol ":" <*> regexSeq <*> symbol ";" <?> "regex"
    where
        f n _ r _ = (n, r)

regexSingle :: Parser Regex
regexSingle = Single <$> Parser.Base.token

regexAny :: Parser Regex
regexAny = Any <$> brackets (some regexSingle)

regexSeq :: Parser Regex
regexSeq = Seq <$> some regexMS

regexMS :: Parser Regex
regexMS = makeExprParser (choice [regexAny, regexSingle]) regexOperatorTable <?> "regex (+ or *)"

regexOperatorTable :: [[Operator Parser Regex]]
regexOperatorTable = 
    [
        [regexPostfix "*" Many],
        [regexPostfix "+" Some]
    ]

regexPostfix :: String -> (Regex -> Regex) -> Operator Parser Regex
regexPostfix name f = Postfix (f <$ symbol name)
    

-------------------------------------------------------------------------------
-- Base parser

nonTerminal :: Parser NonTerminal
nonTerminal =
    NT <$> lexeme ((:) <$> upperChar <*> many letterChar <?> "nonTerminal")

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