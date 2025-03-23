module Syntax.ParsedTree 
    ( ParsedTree(..)
    , flatten
    ) where

import Syntax.Base (Terminal(..), NonTerminal, Pretty(..))
import Text.PrettyPrint.HughesPJ (text, Doc, (<+>), (<>), empty, lbrack, rbrack, hcat)
import Prelude hiding ((<>))
import Data.Generics (Data, Typeable, mkQ, everything)

data ParsedTree
    = ParsedEpsilon
    | ParsedT Terminal
    | ParsedNT NonTerminal ParsedTree
    | ParsedSeq ParsedTree ParsedTree
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
pPrint' indent (ParsedSeq t1 t2) =
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

flatten :: ParsedTree -> String
flatten = everything (++) ("" `mkQ` term)
    where
        term (ParsedT (T t)) = t
        term _ = ""
