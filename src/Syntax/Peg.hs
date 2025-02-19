{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Syntax.Peg where

import Syntax.Base
import Text.PrettyPrint.HughesPJ
import Prelude hiding ((<>))


data Expression
    = Empty
    | ExprT Terminal
    | ExprNT NonTerminal
    | Sequence Expression Expression
    | Choice Expression Expression
    | Star Expression
    | Not Expression
    deriving (Show, Eq)

type Definition = (NonTerminal, Expression) 

type Grammar = ([Definition], NonTerminal)

complex :: Expression -> Bool
complex Empty = False
complex (ExprT _) = False
complex (ExprNT _) = False
complex (Sequence _ _) = True
complex (Choice _ _) = True
complex (Star _) = True
complex (Not _) = True

instance Pretty Expression where
    pPrint :: Expression -> Doc
    pPrint Empty = text "ε"
    pPrint (ExprT t) = pPrint t
    pPrint (ExprNT nt) = pPrint nt
    pPrint (Sequence e1 e2) = pPrint e1 <+> pPrint e2
    pPrint (Choice e1 e2) = pPrint e1 <+> text "/" <+> pPrint e2
    pPrint (Star e) = maybeParens (complex e) (pPrint e) <> text "*"
    pPrint (Not e) = text "!" <> maybeParens (complex e) (pPrint e)

instance Pretty Definition where
    pPrint :: Definition -> Doc
    pPrint (nt, e) = pPrint nt <+> text "<-" <+> pPrint e <+> text "\n"

instance Pretty Grammar where
    pPrint :: Grammar -> Doc
    pPrint (ds, _) = hcat $ map pPrint ds