module Parser.Pattern where

import Syntax.Base (Pretty(pPrint))
import Syntax.Pattern -- (NamedSynPat, SyntaxPattern(..), processPats)
import Parser.Base
    (Parser, sc, hsc, symbol, parens, identifier, nonTerminal, terminal, pSymbol, parseFromFile, parseFromFilePretty)
import Parser.Peg (grammar)
import Text.Megaparsec
    (eof, (<?>), choice, some, parse, errorBundlePretty, sepBy1)
import Text.Megaparsec.Char (char)
import Control.Monad.Combinators.Expr
    (Operator(Postfix, Prefix), makeExprParser)
import Data.Maybe (catMaybes)


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
primary = choice [
                parens expression
            ,   parens patNT
            ,   patT
            ,   metaVariable
            ,   reference
            ,   epsilon
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

-------------------------------------------------------------------------------
--- Testes

parsePat :: FilePath -> IO ()
parsePat = parseFromFile patterns

parsePatPretty :: FilePath -> IO ()
parsePatPretty = parseFromFilePretty patterns

parsePatApply :: Show a => ([NamedSynPat] -> a) -> FilePath -> IO ()
parsePatApply g f = do
        contents <- readFile f
        case parse patterns "" contents of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right xs -> print (g xs)

validPatFile :: FilePath -> FilePath -> IO ()
validPatFile pathGrammar pathPattern = do
    contents_g <- readFile pathGrammar
    contents_p <- readFile pathPattern
    case parse grammar "" contents_g of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right g@(rs, _) ->
            case parse patterns "" contents_p of
                Left bundle -> putStr (errorBundlePretty bundle)
                Right ps ->
                    case processPats g ps of
                        Left bundle -> print bundle
                        Right ps' -> do
                            let proofs = map (validPat' g . snd) ps'
                            let corrected = zipWith correctPat (map snd ps') (catMaybes proofs)
                            -- print (map snd ps')
                            -- print (catMaybes proofs)
                            -- print corrected
                            putStr . show $ pPrint g
                            putStr . show $ pPrint ps
                            putStr . show $ pPrint ps'