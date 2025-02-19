module Parser.Peg where

import Syntax.Base
import Syntax.Peg
import Parser.Base

import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators.Expr


sc' :: Parser ()
sc' = Lexer.space space1 lineComment empty

hsc' :: Parser ()
hsc' = Lexer.space hspace1 lineComment empty

lineComment :: Parser ()
lineComment = () <$ Lexer.skipLineComment "#" <* eol

lexeme' :: Parser a -> Parser a
lexeme' = Lexer.lexeme hsc'

symbol' :: String -> Parser String
symbol' = Lexer.symbol hsc'

parens' :: Parser a -> Parser a
parens' = between (symbol' "(") (symbol' ")")

identifier' :: Parser String
identifier' = lexeme' ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "identifier'")

litString' :: Parser String
litString' = lexeme' (char '"' >> manyTill Lexer.charLiteral (char '"') <?> "string")
            <|> lexeme' (char '\'' >> manyTill Lexer.charLiteral (char '\'') <?> "string")

-------------------------------------------------------------------------------
-- PEG parser

nonTerminal' :: Parser NonTerminal
nonTerminal' =
    NT <$> identifier' <?> "nonTerminal'"

terminal' :: Parser Terminal
terminal' = T <$> litString' <?> "terminal'"

grammar :: Parser Grammar
grammar = f <$> hsc' <*> some definition <* eof
    where
        f _ d = (d, fst $ head d)

definition :: Parser Definition
definition = f <$> nonTerminal' <*> symbol' "<-" <*> pChoice <* sc'
    where
        f nt _ e = (nt, e)

pChoice :: Parser Expression
pChoice = foldr1 Choice <$> Parser.Peg.sequence `sepBy1` symbol' "/"

sequence :: Parser Expression
sequence = foldr1 Sequence <$> some expression

primary :: Parser Expression
primary = choice [
                ExprNT <$> nonTerminal',
                parens' pChoice,
                ExprT <$> terminal',
                pClass,
                dot
            ]

pClass :: Parser Expression
pClass = foldl1 Choice <$> (char '[' >> manyTill range (char ']'))

range :: Parser Expression
range = choice [
            -- f <$> letterChar <*> char '-' <*> letterChar,
            -- f <$> digitChar <*> char '-' <*> digitChar,
            f <$> alphaNumChar <*> char '-' <*> alphaNumChar,
            expr <$> Lexer.charLiteral
        ]
        where
            f a _ b = foldl1 Choice $ map expr [a..b]
            expr = ExprT . T . (:[])

dot :: Parser Expression
dot = ExprNT (NT "") <$ symbol "."

expression :: Parser Expression
expression = makeExprParser primary pegOperatorTable

pegOperatorTable :: [[Operator Parser Expression]]
pegOperatorTable =
    [
        [
            pegSuffix "*" Star,
            pegSuffix "+" plus,
            pegSuffix "?" question
        ],
        [
            pegPrefix "!" Not,
            pegPrefix "&" (Not . Not)
        ]
    ]
    where
        plus e = Sequence e (Star e)
        question e = Choice e Empty

pegSuffix, pegPrefix :: String -> (Expression -> Expression) -> Operator Parser Expression
pegSuffix name f = Postfix (f <$ symbol' name)
pegPrefix name f = Postfix (f <$ symbol' name)

pegBinary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
pegBinary name f = InfixL (f <$ symbol' name)
