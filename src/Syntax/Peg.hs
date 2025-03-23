module Syntax.Peg 
    ( Expression(..)
    , Definition
    , Grammar
    , nonTerminals
    , terminals
    , terminals'
    , expression
    , produces
    ) where

import Syntax.Base (NonTerminal, Terminal, Pretty(..))
import Text.PrettyPrint.HughesPJ (text, Doc, maybeParens, (<+>), (<>), parens)
import Prelude hiding ((<>))
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Generics (Data, Typeable, mkQ, everything)
import Data.Foldable (find)

data Expression
    = Empty
    | ExprT Terminal
    | ExprNT NonTerminal
    | Sequence Expression Expression
    | Choice Expression Expression
    | Star Expression
    | Not Expression
    | Flatten Expression
    deriving (Show, Eq, Ord, Typeable, Data)
{-
TODO: Fazer algo para indentação
    Se for considerar EOF como um não terminal, provavelmente 
    precisa alterar o código de ordenação topológica pra tratar o EOF
    Por enquanto, tá com a ideia de que o EOF é implícito
-}
type Definition = (NonTerminal, Expression)

type Grammar = ([Definition], NonTerminal)

parensSeq :: Expression -> Bool
parensSeq (Choice _ _) = True
parensSeq _ = False

parensNot :: Expression -> Bool
parensNot (Choice _ _) = True
parensNot (Sequence _ _) = True
parensNot _ = False

parensStar :: Expression -> Bool
parensStar (Choice _ _) = True
parensStar (Sequence _ _) = True
parensStar (Not _) = True
parensStar _ = False

instance Pretty Expression where
    pPrint :: Expression -> Doc
    pPrint Empty            = text "ε"
    pPrint (ExprT t)        = pPrint t
    pPrint (ExprNT nt)      = pPrint nt
    pPrint (Sequence e1 e2) = maybeParens (parensSeq e1) (pPrint e1)
                                <+> maybeParens (parensSeq e2) (pPrint e2)
    pPrint (Choice e1 e2)   = pPrint e1 <+> text "/" <+> pPrint e2
    pPrint (Star e)         = maybeParens (parensStar e) (pPrint e) <> text "*"
    pPrint (Not e)          = text "!" <> maybeParens (parensNot e) (pPrint e)
    pPrint (Flatten e)      = text "^" <> parens (pPrint e)

instance Pretty Definition where
    pPrint :: Definition -> Doc
    pPrint (nt, e) = pPrint nt <+> text "<-" <+> pPrint e <> text "\n"

instance Pretty Grammar where
    pPrint :: Grammar -> Doc
    pPrint (ds, _) = pPrint ds

-------------------------------------------------------------------------------

nonTerminals :: Grammar -> [NonTerminal]
nonTerminals (ds, _) = map fst ds

terminals' :: Expression -> [Terminal]
terminals' = nub <$> everything (++) ([] `mkQ` terminal)
    where
        terminal (ExprT t) = [t]
        terminal _ = []

terminals :: Grammar -> [Terminal]
terminals (ds, _) = nub $ concatMap (terminals' . snd) ds

expression :: Grammar -> NonTerminal -> Maybe Expression
expression (rs, _) nt = snd <$> find (\ x -> fst x == nt) rs

produces :: Grammar -> NonTerminal -> Terminal -> Bool
produces g nt t = isJust produce
    where
        expr = expression g nt
        rules = terminals' <$> expr
        produce = find (== t) =<< rules
