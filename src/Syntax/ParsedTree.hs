{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Syntax.ParsedTree where

import Syntax.Base
import Syntax.Pattern
import Text.PrettyPrint.HughesPJ
    (text, Doc, (<+>), (<>), empty, lbrack, rbrack, hcat)
import Prelude hiding ((<>))
import Data.Generics

data ParsedTree
    = ParsedEpsilon
    | ParsedT Terminal
    | ParsedNT NonTerminal ParsedTree
    | ParsedSeq (ParsedTree, ParsedTree)
    | ParsedChoiceLeft ParsedTree
    | ParsedChoiceRight ParsedTree
    | ParsedStar [ParsedTree]
    | ParsedNot
    deriving (Show, Typeable, Data)

instance Pretty ParsedTree where
    pPrint :: ParsedTree -> Doc
    pPrint pt = pPrint' Text.PrettyPrint.HughesPJ.empty pt <> text "\n"

nest :: Doc -> Doc
nest i = i <> text "├╴"

nest1 :: Doc -> Doc
nest1 i = i <> text "╰╴"

continue :: Doc -> Doc
continue i = i <> text "| "

continue1 :: Doc -> Doc
continue1 i = i <> text "  "

pPrint' :: Doc -> ParsedTree -> Doc
pPrint' _ ParsedEpsilon = text "ε"
pPrint' _ (ParsedT t) = pPrint t
pPrint' indent (ParsedNT nt tree) =
    text "NT" <+> pPrint nt <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedSeq (t1, t2)) =
    text "Seq" <> text "\n"
    <> nest indent <> pPrint' (continue indent) t1 <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) t2
pPrint' indent (ParsedChoiceLeft tree) =
    text "Left" <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedChoiceRight tree) =
    text "Right" <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedStar ts) =
    text "Star" <+> lbrack <> list' <> rbrack
    where
        listnest = if null ts then Text.PrettyPrint.HughesPJ.empty else text "\n"
        listEnd   = if null ts then Text.PrettyPrint.HughesPJ.empty else nest1 indent
        list      = hcat (map (\ x -> nest indent <> pPrint' (continue indent) x <> text "\n") ts)
        list'     = listnest <> list <> listEnd
pPrint' _ ParsedNot = Text.PrettyPrint.HughesPJ.empty


match :: Pattern -> ParsedTree -> Bool
match PatEpsilon ParsedEpsilon = True
match (PatNot _) ParsedNot = True
match (PatNT nt p) (ParsedNT nt' t) = nt == nt' && match p t
match (PatT t) (ParsedT t') = t == t'
match (PatVar (Left nt) _) (ParsedNT nt' _) = nt == nt'
match (PatVar (Right t) _) (ParsedT t') = t == t'
match (PatSeq (p1, p2)) (ParsedSeq (t1, t2)) = match p1 t1 && match p2 t2
match (PatChoice p) (ParsedChoiceLeft t) = match p t
match (PatChoice p) (ParsedChoiceRight t) = match p t
match (PatStar p) (ParsedStar ts) = all (match p) ts
-- match (PatVar nt _) (ParsedStar ts) = True
-- match p (ParsedStar ts) = all (match p) ts 
match _ _ = False

collect :: Pattern -> ParsedTree -> [(Pattern, ParsedTree)]
collect PatEpsilon ParsedEpsilon = []
collect (PatNot _) ParsedNot = []
collect (PatNT _ p) (ParsedNT _ t) = collect p t
collect (PatT _) (ParsedT _) = []
collect p@(PatVar (Left nt) _) t@(ParsedNT nt' _) = if nt == nt' then [(p, t)] else []
collect p@(PatVar (Right t) _) tree@(ParsedT t') = if t == t' then [(p, tree)] else []
collect (PatSeq (p1, p2)) (ParsedSeq (t1, t2)) = collect p1 t1 ++ collect p2 t2
collect (PatChoice p) (ParsedChoiceLeft t) = collect p t
collect (PatChoice p) (ParsedChoiceRight t) = collect p t
collect (PatStar p) (ParsedStar ts) = concatMap (collect p) ts
-- collect p@(PatVar nt _) t@(ParsedStar ts) = [(p, t)]
-- collect p t@(ParsedStar _) = [(p, t)]
collect _ _ = []

capture :: Pattern -> ParsedTree -> [(Pattern, ParsedTree)]
capture p = everything (++) ([] `mkQ` (\x -> if match p x then collect p x else []))
