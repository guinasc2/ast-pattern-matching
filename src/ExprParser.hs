module ExprParser where

import Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
-- import Test.Hspec
-- import Test.Hspec.Megaparsec

type Parser = Parsec Void String


sc :: Parser ()
sc = L.space space1 empty empty -- segundo argumento é comentário de linha, terceiro é comentário de bloco

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


-- Exp parser

pVar :: Parser Exp
pVar = 
    Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pNum :: Parser Exp
pNum = Num <$> lexeme L.decimal

pTerm :: Parser Exp
pTerm = choice
    [ parens pExp
    , pVar
    , pNum
    ]

pExp :: Parser Exp
pExp = makeExprParser pTerm expOperatorTable

expOperatorTable :: [[Operator Parser Exp]]
expOperatorTable = 
    [
        [ expBinary "*" Mul]
    ,   [ expBinary "+" Add]
    ]

expBinary :: String -> (Exp -> Exp -> Exp) -> Operator Parser Exp
expBinary  name f = InfixL  (f <$ symbol name)

expPrefix, expPostfix :: String -> (Exp -> Exp) -> Operator Parser Exp
expPrefix  name f = Prefix  (f <$ symbol name)
expPostfix name f = Postfix (f <$ symbol name)




-- Pat parser

pMetaVar :: Parser Pat
pMetaVar =
    MetaVar <$> lexeme ((++) <$> string "#M" <*> many alphaNumChar <?> "meta variable")

pAnyVar :: Parser Pat
pAnyVar =
    AnyVar <$> lexeme ((++) <$> string "#V" <*> many alphaNumChar <?> "meta variable")

pAnyNum :: Parser Pat
pAnyNum =
    AnyNum <$> lexeme ((++) <$> string "#N" <*> many alphaNumChar <?> "meta variable")

pPVar :: Parser Pat
pPVar = 
    PVar <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pPNum :: Parser Pat
pPNum = PNum <$> lexeme L.decimal

pPTerm :: Parser Pat
pPTerm = choice
    [ parens pPat
    , pMetaVar
    , pAnyVar
    , pAnyNum
    , pPVar
    , pPNum
    ]

pPat :: Parser Pat
pPat = makeExprParser pPTerm patOperatorTable

patOperatorTable :: [[Operator Parser Pat]]
patOperatorTable = 
    [
        [ patBinary "*" PMul],
        [ patBinary "+" PAdd]
    ]

patBinary :: String -> (Pat -> Pat -> Pat) -> Operator Parser Pat
patBinary  name f = InfixL  (f <$ symbol name)

patPrefix, patPostfix :: String -> (Pat -> Pat) -> Operator Parser Pat
patPrefix  name f = Prefix  (f <$ symbol name)
patPostfix name f = Postfix (f <$ symbol name)




-- Comb parse

pCombSingle :: Parser Comb
pCombSingle =
    Single <$> lexeme pPat

pCombTerm :: Parser Comb
pCombTerm = choice
    [ parens pComb
    , pCombSingle
    ]

pComb :: Parser Comb
pComb = makeExprParser pCombTerm combOperatorTable

combOperatorTable :: [[Operator Parser Comb]]
combOperatorTable = 
    [
        [combPrefix "~" Not],
        [combBinary "&&" And],
        [combBinary "||" Or]
    ]

combBinary :: String -> (Comb -> Comb -> Comb) -> Operator Parser Comb
combBinary  name f = InfixL  (f <$ symbol name)

combPrefix, combPostfix :: String -> (Comb -> Comb) -> Operator Parser Comb
combPrefix  name f = Prefix  (f <$ symbol name)
combPostfix name f = Postfix (f <$ symbol name)




-------------------------------------------------------------------------------


matchTest :: String -> String -> Either String (Maybe [(String, Exp)])
matchTest p e =
    case parse pPat "" p of
        Left bundle -> Left ("Error on pattern: " ++ errorBundlePretty bundle)
        Right p' -> case parse pExp "" e of
            Left bundle -> Left ("Error on expression: " ++ errorBundlePretty bundle)
            Right e' -> Right (deepMatchTree p' e')
    -- do
    --     p' <- parseMaybe (pPat <* eof) p
    --     e' <- parseMaybe (pExp <* eof) e
    --     matchTree p' e'

matchTest' :: String -> String -> Either String Bool
matchTest' c e =
    case parse pComb "" c of
        Left bundle -> Left ("Error on pattern(s): " ++ errorBundlePretty bundle)
        Right c' -> case parse pExp "" e of
            Left bundle -> Left ("Error on expression: " ++ errorBundlePretty bundle)
            Right e' -> Right (matchComb c' e')
    -- do
    --     c' <- parseMaybe (pPat <* eof) c
    --     e' <- parseMaybe (pExp <* eof) e
    --     matchTree c' e'