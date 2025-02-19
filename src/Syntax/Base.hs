{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax.Base where

import Text.PrettyPrint.HughesPJ

data NonTerminal
    = NT String
    deriving (Eq, Show, Ord)
data Terminal
    = T String
    deriving (Eq, Show, Ord)

type Symbol = Either NonTerminal Terminal

class Pretty a where
    pPrint :: a -> Doc

instance Pretty NonTerminal where
    pPrint :: NonTerminal -> Doc
    pPrint (NT nt) = text nt

instance Pretty Terminal where
    pPrint :: Terminal -> Doc
    pPrint (T t) = text ("\"" ++ t ++ "\"")

instance Pretty Symbol where
    pPrint :: Symbol -> Doc
    pPrint (Left nt) = pPrint nt
    pPrint (Right t) = pPrint t


-- TODO: Ainda vai precisar dessas coisas de token e regex?
data Token
    = Identifier
    | Digit
    | Integer
    | Double
    | SignedInteger
    | SignedDouble
    | Num
    | SignedNum
    | StringLiteral -- qualquer string
    | Exact String -- string entre aspas significa que deve casar somente ela
    | RegRef String
    deriving (Show, Eq, Ord)

data Regex
    = Single Token
    | Any [Regex]
    | Seq [Regex]
    | Many Regex
    | Some Regex
    deriving (Show, Eq, Ord)

type NamedRegex = (String, Regex)


-------------------------------------------------------------------------------

showS :: Symbol -> String
showS (Left (NT nt)) = nt ++ " "
showS (Right (T t)) = "\"" ++ t ++ "\" "