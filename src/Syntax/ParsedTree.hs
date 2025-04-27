{-|
Module      : Syntax.ParsedTree
Description : Representation of parsed trees.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the structure of a parsed tree ('ParsedTree') and
associated functions, such as the 'flatten' function to extract the terminals from a tree.
It also provides an instance of the 'Pretty' class for formatted printing.
-}
module Syntax.ParsedTree 
    ( ParsedTree(..)
    , flatten
    ) where

import Syntax.Base (Terminal(..), NonTerminal, Pretty(..))
import Text.PrettyPrint.HughesPJ (text, Doc, (<+>), (<>), empty, lbrack, rbrack, hcat)
import Prelude hiding ((<>))
import Data.Generics (Data, Typeable, mkQ, everything)

{-|
Represents an abstract syntax tree (AST).

A 'ParsedTree' can be:
- 'ParsedEpsilon': Represents the empty tree (ε).
- 'ParsedT': A terminal symbol.
- 'ParsedNT': A non-terminal symbol associated with a subtree.
- 'ParsedSeq': A sequence of two trees.
- 'ParsedChoiceLeft': Represents the left choice in a choice operation.
- 'ParsedChoiceRight': Represents the right choice in a choice operation.
- 'ParsedStar': Represents a repetition of zero or more times of a tree.
- 'ParsedNot': Represents the negation of a tree.

@since 1.0.0
-}
data ParsedTree
    = ParsedEpsilon
    | ParsedT Terminal
    | ParsedNT NonTerminal ParsedTree
    | ParsedSeq ParsedTree ParsedTree
    | ParsedChoiceLeft ParsedTree
    | ParsedChoiceRight ParsedTree
    | ParsedStar [ParsedTree]
    | ParsedNot
    | ParsedIndent ParsedTree ParsedTree
    deriving (Show, Typeable, Data)

{-|
Instance of the 'Pretty' class for 'ParsedTree'.

Prints the syntax tree in a readable format, with indentation
and visual symbols to represent the tree hierarchy.

@since 1.0.0
-}
instance Pretty ParsedTree where
    pPrint :: ParsedTree -> Doc
    pPrint pt = pPrint' Text.PrettyPrint.HughesPJ.empty pt <> text "\n"

-- Auxiliary functions for tree formatting
nest :: Doc -> Doc
nest i = i <> text "├╴"

nest1 :: Doc -> Doc
nest1 i = i <> text "╰╴"

continue :: Doc -> Doc
continue i = i <> text "| "

continue1 :: Doc -> Doc
continue1 i = i <> text "  "

{-|
Auxiliary function for formatted printing of a 'ParsedTree'.

@since 1.0.0
-}
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
pPrint' indent (ParsedIndent e b) =
    text "Indent" <> text "\n"
    <> nest indent <> pPrint' (continue indent) e <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) b

{-|
Extracts all terminal symbols from a 'ParsedTree' as a single string.

=== Usage examples:

>>> flatten (ParsedSeq (ParsedT (T "a")) (ParsedT (T "b")))
"ab"

>>> flatten ParsedEpsilon
""

@since 1.0.0
-}
flatten :: ParsedTree -> String
flatten = everything (++) ("" `mkQ` term)
    where
        term (ParsedT (T t)) = t
        term _ = ""
