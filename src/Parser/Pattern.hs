module Parser.Pattern 
    ( patterns
    , parsePatterns
    ) where

import Syntax.Pattern (SyntaxPattern(..), NamedSynPat)
import Parser.Base
    (Parser, sc, hsc, symbol, parens, identifier, nonTerminal, terminal, pSymbol, parseWith, ParseError)
import Text.Megaparsec (eof, (<?>), choice, some, sepBy1)
import Text.Megaparsec.Char (char)
import Control.Monad.Combinators.Expr (Operator(Postfix, Prefix), makeExprParser)


-------------------------------------------------------------------------------
--- SyntaxPattern Parser

patterns :: Parser [NamedSynPat]
patterns = id <$ hsc <*> some pat <* eof

pat :: Parser NamedSynPat
pat =
    f <$> symbol "pattern"
        <*> identifier
        <*> symbol ":"
        <*> patNT
        <* sc
        <?> "pattern"
    where
        f _ name _ p = (name, p)

primary :: Parser SyntaxPattern
primary = choice 
            [ parens expression
            , parens patNT
            , patT
            , metaVariable
            , reference
            , epsilon
            ]

expression :: Parser SyntaxPattern
expression = foldr1 SynChoice <$> Parser.Pattern.sequence `sepBy1` symbol "/"

sequence :: Parser SyntaxPattern
sequence = foldr1 SynSeq <$> some prefix

patNT :: Parser SyntaxPattern
patNT = f <$> nonTerminal <*> symbol ":=" <*> expression
    where f nt _ = SynNT nt

patT :: Parser SyntaxPattern
patT = SynT <$> terminal

metaVariable :: Parser SyntaxPattern
metaVariable = f <$> char '#' <*> identifier <*> char ':' <*> pSymbol <?> "meta variable"
    where
        f _ n _ s = SynVar s n

reference :: Parser SyntaxPattern
reference = f <$> char '@' <*> identifier <?> "pattern name"
    where
        f _ = SynRef

epsilon :: Parser SyntaxPattern
epsilon = SynEpsilon <$ symbol "ε"

prefix :: Parser SyntaxPattern
prefix = makeExprParser primary patOperatorTable

patOperatorTable :: [[Operator Parser SyntaxPattern]]
patOperatorTable =
    [
        [
            patSuffix "*" SynStar,
            patSuffix "+" plus,
            patSuffix "?" question
        ],
        [
            patPrefix "!" SynNot,
            patPrefix "&" (SynNot . SynNot)
        ]
    ]
    where
        plus e = SynSeq e (SynStar e)
        question e = SynChoice e SynEpsilon


patSuffix, patPrefix :: String -> (SyntaxPattern -> SyntaxPattern) -> Operator Parser SyntaxPattern
patSuffix name f = Postfix (f <$ symbol name)
patPrefix name f = Prefix (f <$ symbol name)

parsePatterns :: String -> Either ParseError [NamedSynPat]
parsePatterns = parseWith patterns
