module Parser.Pattern where

import Syntax.Base (Pretty(pPrint))
import Syntax.Pattern -- (NamedSynPat, SyntaxPattern(..), processPats)
import Parser.Base
    (Parser, sc, hsc, symbol, parens, identifier, nonTerminal, terminal, pSymbol, parseFromFile, parseFromFilePretty)
import Parser.Peg (grammar)
import Text.Megaparsec
    (eof, (<?>), choice, some, parse, errorBundlePretty)
import Text.Megaparsec.Char (char)
import Control.Monad.Combinators.Expr
    (Operator(Postfix, Prefix), makeExprParser)


-------------------------------------------------------------------------------
--- SyntaxPattern Parser

-- TODO: não tem um parser para SynChoice ainda

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
                parens Parser.Pattern.sequence
            ,   parens patNT
            ,   patT
            ,   patVar
            ,   patRef
            ,   patEpsilon
            ]

sequence :: Parser SyntaxPattern
sequence = foldr1 (curry SynSeq) <$> some prefix

patNT :: Parser SyntaxPattern
patNT = f <$> nonTerminal <*> symbol ":=" <*> Parser.Pattern.sequence
    where f nt _ = SynNT nt

patT :: Parser SyntaxPattern
patT = SynT <$> terminal

patVar :: Parser SyntaxPattern
patVar = f <$> char '#' <*> identifier <*> char ':' <*> pSymbol <?> "meta variable"
    where
        f _ n _ s = SynVar s n

patRef :: Parser SyntaxPattern
patRef = f <$> char '@' <*> identifier <?> "pattern name"
    where
        f _ = SynRef

patEpsilon :: Parser SyntaxPattern
patEpsilon = SynEpsilon <$ symbol "ε"

prefix :: Parser SyntaxPattern
prefix = makeExprParser primary patOperatorTable

patOperatorTable :: [[Operator Parser SyntaxPattern]]
patOperatorTable =
    [
        [
            patSuffix "*" SynStar,
            patSuffix "+" plus --,
            -- patSuffix "?" question
        ],
        [
            patPrefix "!" SynNot,
            patPrefix "&" (SynNot . SynNot)
        ]
    ]
    where
        plus e = SynSeq (e, SynStar e)
        -- question e = PatChoice e Empty -- Seria PatChoice, mas ele só recebe um parâmetro


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
                        Left bundle -> do
                            print bundle
                        Right ps' -> do
                            -- print rs
                            putStr . show $ pPrint g
                            putStr . show $ pPrint ps
                            putStr . show $ pPrint ps'