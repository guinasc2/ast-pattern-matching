module Syntax.Pattern 
    ( Pattern(..)
    , SyntaxPattern(..)
    , NamedPattern
    , NamedSynPat
    , references
    , replaceSynPats
    ) where

import Syntax.Base (NonTerminal(..), Terminal(..), Symbol, Pretty(..))
import Text.PrettyPrint.HughesPJ ((<+>), text, parens, Doc)
import Data.List (nub)
import Data.Bifunctor (Bifunctor(second))
import Data.Generics (everything, mkQ, Typeable, Data, everywhere, mkT)

data Pattern
    = PatEpsilon
    | PatT Terminal
    | PatNT NonTerminal Pattern
    | PatSeq Pattern Pattern
    | PatChoice Pattern Pattern
    | PatStar Pattern
    | PatNot Pattern
    | PatVar Symbol String
    deriving (Eq, Show, Ord, Typeable, Data)

data SyntaxPattern
    = SynEpsilon
    | SynT Terminal
    | SynNT NonTerminal SyntaxPattern
    | SynSeq SyntaxPattern SyntaxPattern
    | SynChoice SyntaxPattern SyntaxPattern
    | SynStar SyntaxPattern
    | SynNot SyntaxPattern
    | SynVar Symbol String
    | SynRef String
    deriving (Eq, Show, Ord, Typeable, Data)

type NamedPattern = (String, Pattern)
type NamedSynPat = (String, SyntaxPattern)

instance Pretty Pattern where
    pPrint :: Pattern -> Doc
    pPrint (PatNT nt p)      = pPrint nt <+> text ":=" <+> (parens . pPrint) p
    pPrint (PatT t)          = pPrint t
    pPrint (PatVar s name)   = text ("#" ++ name) <> text ":" <> pPrint s
    pPrint PatEpsilon        = text "ε"
    pPrint (PatSeq p1 p2)    = pPrint p1 <+> pPrint p2
    pPrint (PatChoice p1 p2) = pPrint p1 <+> text "/" <+> pPrint p2
    pPrint (PatStar p)       = (parens . pPrint) p <> text "*"
    pPrint (PatNot p)        = text "!" <> (parens . pPrint) p

instance Pretty SyntaxPattern where
    pPrint :: SyntaxPattern -> Doc
    pPrint (SynNT nt ps)     = pPrint nt <+> text ":=" <+> (parens . pPrint) ps
    pPrint (SynT t)          = pPrint t
    pPrint (SynVar s name)   = text ("#" ++ name) <> text ":" <> pPrint s
    pPrint SynEpsilon        = text "ε"
    pPrint (SynSeq p1 p2)    = pPrint p1 <+> pPrint p2
    pPrint (SynChoice p1 p2) = pPrint p1 <+> text "/" <+> pPrint p2
    pPrint (SynStar p)       = (parens . pPrint) p <> text "*"
    pPrint (SynNot p)        = text "!" <> (parens . pPrint) p
    pPrint (SynRef name)     = text $ "@" ++ name

instance Pretty NamedPattern where
    pPrint :: NamedPattern -> Doc
    pPrint (name, pat) = text ("pattern " ++ name ++ " :") <+> pPrint pat <+> text "\n"

instance Pretty NamedSynPat where
    pPrint :: NamedSynPat -> Doc
    pPrint (name, syn) = text ("pattern " ++ name ++ " :") <+> pPrint syn <+> text "\n"

-------------------------------------------------------------------------------

replaceInPat :: NamedSynPat -> SyntaxPattern -> SyntaxPattern
replaceInPat ref = everywhere $ mkT (replace ref)
    where
        replace (n, p) (SynRef n') = if n == n' then p else SynRef n'
        replace _ p                = p

references :: SyntaxPattern -> [String]
references = nub <$> everything (++) ([] `mkQ` refs)
    where
        refs (SynRef s) = [s]
        refs _ = []

replaceSynPats :: [NamedSynPat] -> [NamedSynPat]
replaceSynPats ps = foldr replaceSynPat ps ps

replaceSynPat :: NamedSynPat -> [NamedSynPat] -> [NamedSynPat]
replaceSynPat p = map (second $ replaceInPat p)
