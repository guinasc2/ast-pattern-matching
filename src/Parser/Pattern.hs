{-# LANGUAGE TupleSections #-}

module Parser.Pattern where

import Syntax.Base
import Syntax.Grammar
import Syntax.Pattern

import Parser.Base
import Parser.Grammar

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer


-------------------------------------------------------------------------------
-- SyntaxPattern Parser

pPat :: Parser NamedSynPat
pPat = f <$> symbol "pattern" <*> identifier <*> symbol ":" <*> choice [pNonTerminal, pTerminal, pVar] <?> "pattern"
    where
        f _ name _ p = (name, p)

pPat' :: Parser SyntaxPat
pPat' = choice [parens pNonTerminal, pTerminal, pVar, pRef]

pNonTerminal :: Parser SyntaxPat
pNonTerminal = f <$> nonTerminal <*> symbol ":=" <*> many pPat'
    where f nt _ ps = SynNT nt ps

pTerminal :: Parser SyntaxPat
pTerminal = SynT <$> terminal

pVar :: Parser SyntaxPat
pVar = f <$> char '#' <*> identifier <*> char ':' <*> pSymbol <?> "meta variable"
    where
        f _ n _ s = SynVar s n

pRef :: Parser SyntaxPat
pRef = f <$> char '@' <*> identifier <?> "pattern name"
    where
        f _ n = SynRef n

-------------------------------------------------------------------------------
-- File Parser

patternFile :: Parser [NamedSynPat]
patternFile = some pPat <* eof

-------------------------------------------------------------------------------
--- Testes

parsePat :: FilePath -> IO ()
parsePat f = do
        contents <- readFile f
        case parse (many pPat) "" contents of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right xs -> putStr (concatMap g xs)
                where
                    g (i, p) = "pattern " ++ i ++ " : " ++ showP' p ++ "\n"

parsePatApply :: Show a => ([NamedSynPat] -> a) -> FilePath -> IO ()
parsePatApply g f = do
        contents <- readFile f
        case parse (many pPat) "" contents of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right xs -> print (g xs)

validPatFile :: FilePath -> FilePath -> IO ()
validPatFile pathGrammar pathPattern = do
    contents_g <- readFile pathGrammar
    contents_p <- readFile pathPattern
    case parse grammarFile "" contents_g of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right (rs, g) ->
            case parse patternFile "" contents_p of
                Left bundle -> putStr (errorBundlePretty bundle)
                Right ps -> 
                    case processPats g ps of
                        Left bundle -> print bundle
                        Right ps' -> do
                            print rs
                            putStrLn (showG g)
                            putStrLn (concatMap printNamedSyn ps)
                            putStrLn (concatMap printNamedPat ps')
    where
        printNamedSyn (i, p) = "pattern " ++ i ++ " : " ++ showP' p ++ "\n"
        printNamedPat (i, p) = "pattern " ++ i ++ " : " ++ showP p ++ "\n"
