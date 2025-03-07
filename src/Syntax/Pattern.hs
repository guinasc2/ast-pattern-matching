{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax.Pattern where

import Syntax.Base (NonTerminal(..), Terminal(..), Symbol, Pretty(..))
import Syntax.Peg
    (Grammar, Expression(..), nonTerminals, terminals, expression)
import Text.PrettyPrint.HughesPJ ((<+>), text, parens, hcat, Doc)
import Data.Foldable (Foldable (toList))
import Data.Maybe (catMaybes, isNothing, fromJust, fromMaybe)
import Data.Either (rights, lefts)
import Data.List (nub)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import Data.Bifunctor (Bifunctor(second, first))
import Control.Exception (Exception)

data Pattern
    = PatEpsilon
    | PatT Terminal
    | PatNT NonTerminal Pattern
    | PatSeq (Pattern, Pattern)
    | PatChoice Pattern
    | PatStar Pattern
    | PatNot Pattern 
    | PatVar Symbol String
    deriving (Eq, Show, Ord)

data SyntaxPattern
    = SynEpsilon
    | SynT Terminal
    | SynNT NonTerminal SyntaxPattern
    | SynSeq (SyntaxPattern, SyntaxPattern)
    | SynChoice SyntaxPattern
    | SynStar SyntaxPattern
    | SynNot SyntaxPattern
    | SynVar Symbol String
    | SynRef String
    deriving (Eq, Show, Ord)

type NamedPattern = (String, Pattern)
type NamedSynPat = (String, SyntaxPattern)

instance Pretty Pattern where
    pPrint :: Pattern -> Doc
    pPrint (PatNT nt p) = pPrint nt <+> text ":=" <+> (parens . pPrint) p
    pPrint (PatT t) = pPrint t
    pPrint (PatVar s name) = text ("#" ++ name) <> text ":" <> pPrint s
    pPrint PatEpsilon = text "ε"
    pPrint (PatSeq (p1, p2)) = pPrint p1 <+> pPrint p2
    pPrint (PatChoice p) = pPrint p
    pPrint (PatStar p) = (parens . pPrint) p <> text "*"
    pPrint (PatNot p) = text "!" <> (parens . pPrint) p

instance Pretty SyntaxPattern where
    pPrint :: SyntaxPattern -> Doc
    pPrint (SynNT nt ps) = pPrint nt <+> text ":=" <+> (parens . pPrint) ps
    pPrint (SynT t) = pPrint t
    pPrint (SynVar s name) = text ("#" ++ name) <> text ":" <> pPrint s
    pPrint SynEpsilon = text "ε"
    pPrint (SynSeq (p1, p2)) = pPrint p1 <+> pPrint p2
    pPrint (SynChoice p) = pPrint p
    pPrint (SynStar p) = (parens . pPrint) p <> text "*"
    pPrint (SynNot p) = text "!" <> (parens . pPrint) p
    pPrint (SynRef name) = text $ "@" ++ name

instance Pretty NamedPattern where
    pPrint :: NamedPattern -> Doc
    pPrint (name, pat) = text ("pattern " ++ name ++ " :") <+> pPrint pat <+> text "\n"

instance Pretty NamedSynPat where
    pPrint :: NamedSynPat -> Doc
    pPrint (name, syn) = text ("pattern " ++ name ++ " :") <+> pPrint syn <+> text "\n"

data RefOutOfScopeException
    = RefOutOfScope NamedSynPat [String]
    deriving (Show, Eq, Ord)

data InvalidPatternException
    = InvalidSyntax NamedSynPat
    | InvalidPattern NamedPattern
    deriving (Show, Eq, Ord)

data DuplicatePatternException
    = DuplicatePattern String [SyntaxPattern]
    deriving (Show, Eq, Ord)

data PatternException
    = PatOutOfScope [RefOutOfScopeException]
    | PatRecursive [NamedSynPat]
    | PatInvalid [InvalidPatternException]
    | PatDuplicate [DuplicatePatternException]
    deriving (Show, Eq, Ord)

instance Exception RefOutOfScopeException
instance Exception InvalidPatternException
instance Exception DuplicatePatternException
instance Exception PatternException

-------------------------------------------------------------------------------

synToPat :: SyntaxPattern -> Maybe Pattern
synToPat SynEpsilon = Just PatEpsilon
synToPat (SynNot p) = PatNot <$> synToPat p
synToPat (SynNT nt p) = PatNT nt <$> synToPat p
synToPat (SynT t) = Just $ PatT t
synToPat (SynVar s n) = Just $ PatVar s n
synToPat (SynSeq (p1, p2)) = curry PatSeq <$> synToPat p1 <*> synToPat p2
synToPat (SynChoice p) = PatChoice <$> synToPat p
synToPat (SynStar p) = PatStar <$> synToPat p
synToPat (SynRef _) = Nothing

synToPat' :: NamedSynPat -> Either InvalidPatternException NamedPattern
synToPat' p@(n, sn) = maybeToRight (synToPat sn)
    where
        invalid = Left $ InvalidSyntax p
        maybeToRight = maybe invalid (Right . (n,))

replaceInPat :: NamedSynPat -> SyntaxPattern -> SyntaxPattern
replaceInPat (n, p) (SynRef n') = if n == n' then p else SynRef n'
replaceInPat p (SynNT nt p') = SynNT nt $ replaceInPat p p'
replaceInPat p (SynSeq (p1, p2)) = SynSeq (replaceInPat p p1, replaceInPat p p2)
replaceInPat p (SynChoice p') = SynChoice $ replaceInPat p p'
replaceInPat p (SynStar p') = SynStar $ replaceInPat p p'
replaceInPat p (SynNot p') = SynNot $ replaceInPat p p'
replaceInPat _ p = p

references :: SyntaxPattern -> [String]
references (SynNT _ ps) = references ps
references (SynRef s) = [s]
references (SynSeq (p1, p2)) = references p1 ++ references p2
references (SynChoice p) = references p
references (SynStar p) = references p
references (SynNot p) = references p
references _ = []

dependencies ::
    NamedSynPat
    -> [NamedSynPat]
    -> Either RefOutOfScopeException [(NamedSynPat, NamedSynPat)] -- Lista de arestas
dependencies p ps =
    case filter (isNothing . snd) depends of
        [] -> Right $ map (\ x -> (second fromJust x, p)) depends
        x -> Left $ RefOutOfScope p (map fst x)
    where
        refs = nub . references $ snd p
        findPats ps' s = (s, lookup s ps')
        depends = map (findPats ps) refs

mkEdges :: [NamedSynPat] -> Either PatternException [(NamedSynPat, NamedSynPat)]
mkEdges ps =
    case lefts result of
        [] -> Right . concat . rights $ result
        p -> Left . PatOutOfScope $ p
    where
        result = map (`dependencies` ps) ps

duplicates :: [NamedSynPat] -> Either PatternException [NamedSynPat]
duplicates ps =
    case duplicates' ps [] [] of
        [] -> Right ps
        ds -> Left . PatDuplicate $ map (\ x -> DuplicatePattern x (filterEqual ps x)) ds
    where
        duplicates' [] _ dups = dups
        duplicates' (x:xs) checked dups =
            if fst x `elem` checked
                then duplicates' xs checked (fst x:dups)
                else duplicates' xs (fst x:checked) dups
        filterEqual ps' x = map snd $ filter (\ p -> x == fst p) ps'

mkGraph :: [NamedSynPat] -> Either PatternException (Alga.AdjacencyMap NamedSynPat)
mkGraph ps =
    case duplicates ps of
        Left e -> Left e
        Right ps' -> second (Alga.overlay (Alga.vertices ps) . Alga.edges) (mkEdges ps')

topSort :: Alga.AdjacencyMap NamedSynPat -> Either PatternException [NamedSynPat]
topSort g = first (PatRecursive . toList) (Algo.topSort g)

replacePats :: [NamedSynPat] -> [NamedSynPat]
replacePats ps = foldr replacePat ps ps

replacePat :: NamedSynPat -> [NamedSynPat] -> [NamedSynPat]
replacePat p = map (second $ replaceInPat p)

-------------------------------------------------------------------------------
--- Validation

validPat :: Grammar -> Pattern -> Bool
validPat g (PatNT nt p) = fromMaybe False rule
    where
        expr = expression g nt
        rule = checkPat g p <$> expr
validPat g@(ds, _) p = any (checkPat g p . snd) ds

checkPat :: Grammar -> Pattern -> Expression -> Bool
checkPat _ PatEpsilon           Empty            = True
checkPat _ (PatT t)             (ExprT t')       = t == t'
checkPat _ (PatVar (Right t) _) (ExprT t')       = t == t'
checkPat g p@(PatNT nt _)       (ExprNT nt')     = nt == nt' && validPat g p
checkPat _ (PatVar (Left nt) _) (ExprNT nt')     = nt == nt'
checkPat g (PatSeq (p1, p2))    (Sequence e1 e2) = checkPat g p1 e1 && checkPat g p2 e2
checkPat g (PatChoice p)        (Choice e1 e2)   = checkPat g p e1 || checkPat g p e2
checkPat g p                    (Choice e1 e2)   = checkPat g p e1 || checkPat g p e2
checkPat g (PatStar p)          (Star e)         = checkPat g p e
-- checkPat g (PatStar p)          e                = checkPat g p e
-- checkPat g p                    (Star e)         = checkPat g p e
checkPat g (PatNot p)           (Not e)          = checkPat g p e
-- checkPat g (PatNot p)           e                = checkPat g p e
-- checkPat g p                    (Not e)          = checkPat g p e
checkPat _ _                    _                = False

validSynPats :: Grammar -> [NamedSynPat] -> Either PatternException [NamedPattern]
validSynPats g ps =
    case lefts pats of
        [] -> case filter (not . validPat g . snd) valids of
                [] -> Right valids
                invalids -> Left $ PatInvalid (map InvalidPattern invalids)
        e -> Left $ PatInvalid e
    where
        pats = map synToPat' ps
        valids = rights pats

processPats :: Grammar -> [NamedSynPat] -> Either PatternException [NamedPattern]
processPats g ps =
    case mkGraph ps of
        Left x -> Left x
        Right graph ->
            case topSort graph of
                Left x -> Left x
                Right ps' -> validSynPats g $ replacePats ps'