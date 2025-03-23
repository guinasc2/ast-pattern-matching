{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Syntax.Base 
    ( NonTerminal(..)
    , Terminal(..)
    , Symbol
    , Pretty(..)
    , toMaybe
    , duplicatesOfFirst
    , filterByFirst  
    ) where

import Text.PrettyPrint.HughesPJ (text, Doc, hcat)
import Data.Generics (Data, Typeable)

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
    pPrint (T t) = text (show t)

instance Pretty Symbol where
    pPrint :: Symbol -> Doc
    pPrint (Left nt) = pPrint nt
    pPrint (Right t) = pPrint t

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a = Just a

duplicatesOfFirst :: Eq a => [(a, b)] -> [a]
duplicatesOfFirst ls = duplicates ls [] []
    where
        duplicates []     _       dups = dups
        duplicates (x:xs) checked dups =
            if fst x `elem` checked
                then duplicates xs checked (fst x:dups)
                else duplicates xs (fst x:checked) dups

filterByFirst :: Eq a => [(a, b)] -> a -> [b]
filterByFirst g' x = map snd $ filter ((x ==) . fst) g'
