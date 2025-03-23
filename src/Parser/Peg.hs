module Parser.Peg 
    ( grammar
    , parseGrammar
    ) where

import Syntax.Base (NonTerminal(NT), Terminal(..))
import Syntax.Peg (Grammar, Definition, Expression(..))
import Parser.Base
    (Parser, sc, symbol, parens, nonTerminal, terminal, hsc, parseWith, ParseError)
import Text.Megaparsec
    (eof, choice, some, sepBy1, someTill, try, optional)
import Text.Megaparsec.Char (alphaNumChar, char, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators.Expr (Operator(Postfix, Prefix), makeExprParser)


-------------------------------------------------------------------------------
--- PEG parser

grammar :: Parser Grammar
grammar = f <$> sc <*> some definition <* eof
    where
        f _ d = (d, fst $ head d)

definition :: Parser Definition
definition = f <$> optional (string "^") <*> nonTerminal <*> symbol "<-" <*> expression <* sc
    where
        f flat nt _ e = case flat of
                        Nothing -> (nt, e)
                        Just _  -> (nt, Flatten e)

expression :: Parser Expression
expression = foldr1 Choice <$> Parser.Peg.sequence `sepBy1` symbol "/"

sequence :: Parser Expression
sequence = foldr1 Sequence <$> some prefix

primary :: Parser Expression
primary = choice 
            [ epsilon
            , ExprNT <$> nonTerminal
            , parens expression
            , ExprT <$> terminal
            , pClass
            , dot
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
dot = ExprNT (NT ".") <$ symbol "."

epsilon :: Parser Expression
epsilon = Empty <$ symbol "ε"

prefix :: Parser Expression
prefix = makeExprParser primary pegOperatorTable

pegOperatorTable :: [[Operator Parser Expression]]
pegOperatorTable =
    [ [ pegSuffix "*" Star
      , pegSuffix "+" plus
      , pegSuffix "?" question
      ]
    , [ pegPrefix "!" Not
      , pegPrefix "&" (Not . Not)
      ]
    ]
    where
        plus e = Sequence e (Star e)
        question e = Choice e Empty

pegSuffix, pegPrefix :: String -> (Expression -> Expression) -> Operator Parser Expression
pegSuffix name f = Postfix (f <$ symbol name)
pegPrefix name f = Prefix (f <$ symbol name)

parseGrammar :: String -> Either ParseError Grammar
parseGrammar = parseWith grammar
