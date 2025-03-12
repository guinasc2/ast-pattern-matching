{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Syntax.Base where

import Text.PrettyPrint.HughesPJ (text, Doc, hcat)
import Data.Generics

data NonTerminal
    = NT String
    deriving (Eq, Show, Ord, Typeable, Data)
data Terminal
    = T String
    deriving (Eq, Show, Ord, Typeable, Data)

type Symbol = Either NonTerminal Terminal

class Pretty a where
    pPrint :: a -> Doc

instance Pretty a => Pretty [a] where
    pPrint :: [a] -> Doc
    pPrint l = hcat (map pPrint l)

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

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a = Just a
