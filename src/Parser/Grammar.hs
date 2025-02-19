{-# LANGUAGE TupleSections #-}

module Parser.Grammar where

import Syntax.Base
import Syntax.Grammar
import Parser.Base
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------
-- Grammar parser

rhs :: Parser RHS
rhs = some pSymbol

rule :: Parser [Rule]
rule = f <$> nonTerminal <*> symbol "->" <*> rhs `sepBy1` symbol "|" <*> symbol ";"
    where
        f nt _ r _ = map (nt,) r

grammar :: Parser Grammar
grammar = f <$> some rule <* eof
    where
        f x = (concat x, (fst . head . concat) x)

-------------------------------------------------------------------------------
-- Grammar File Parser

grammarFile :: Parser ([NamedRegex], Grammar)
grammarFile = f <$> optional (const <$> many regex <*> symbol "%%") <*> grammar
    where
        f rs g = (fromMaybe [] rs, g)

-------------------------------------------------------------------------------
--- Testes

parseGrammar :: FilePath -> IO ()
parseGrammar f = do
        contents <- readFile f
        case parse grammar "" contents of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right xs -> putStr (showG xs)

parseApply :: Show a => (Grammar -> a) -> FilePath -> IO ()
parseApply g f = do
        contents <- readFile f
        case parse grammar "" contents of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right xs -> print (g xs)
