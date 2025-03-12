module Parser.Peg where

import Syntax.Base (NonTerminal(NT), Terminal(..), pPrint)
import Syntax.Peg
    (Grammar, Definition, Expression(..), processPeg)
import Parser.Base
    (Parser, sc, symbol, parens, nonTerminal, terminal, parseFromFile, parseFromFilePretty, hsc, brackets)
import Text.Megaparsec
    (eof, choice, some, errorBundlePretty, sepBy1, parse, someTill, try, optional)
import Text.Megaparsec.Char (alphaNumChar, char)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators.Expr
    (Operator(Postfix, Prefix), makeExprParser)


-------------------------------------------------------------------------------
--- PEG parser

grammar :: Parser Grammar
grammar = f <$> sc <*> some definition <* eof
    where
        f _ d = (d, fst $ head d)

definition :: Parser Definition
definition = f <$> nonTerminal <*> symbol "<-" <*> expression <* sc
    where
        f nt _ e = (nt, e)

expression :: Parser Expression
expression = foldr1 Choice <$> Parser.Peg.sequence `sepBy1` symbol "/"

sequence :: Parser Expression
sequence = foldr1 Sequence <$> some prefix

primary :: Parser Expression
primary = choice [
                epsilon,
                ExprNT <$> nonTerminal,
                parens expression,
                ExprT <$> terminal,
                pClass,
                dot
            ]

pClass :: Parser Expression
pClass = foldr1 Choice <$> (char '[' >> someTill range (char ']')) <* hsc

range :: Parser Expression
range = choice [
            try $ f <$> alphaNumChar <*> char '-' <*> alphaNumChar,
            expr <$> Lexer.charLiteral
        ]
        where
            f a _ b = foldr1 Choice $ map expr [a..b]
            expr = ExprT . T . (:[])

dot :: Parser Expression
dot = ExprNT (NT "") <$ symbol "."

epsilon :: Parser Expression
epsilon = Empty <$ symbol "ε"

prefix :: Parser Expression
prefix = makeExprParser primary pegOperatorTable

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
pegSuffix name f = Postfix (f <$ symbol name)
pegPrefix name f = Prefix (f <$ symbol name)


-------------------------------------------------------------------------------
--- Testes

parseGrammar :: FilePath -> IO ()
parseGrammar = parseFromFile grammar

parseGrammarPretty :: FilePath -> IO ()
parseGrammarPretty = parseFromFilePretty grammar

parseGrammar' :: FilePath -> IO ()
parseGrammar' f = do
        contents <- readFile f
        case parse grammar "" contents of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right g ->
                case processPeg g of
                    Left bundle -> print bundle
                    Right g' -> putStr . show $ pPrint g'