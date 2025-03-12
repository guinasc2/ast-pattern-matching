{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax.Pattern where

import Syntax.Base (NonTerminal(..), Terminal(..), Symbol, Pretty(..), toMaybe)
import Syntax.Peg
    (Grammar, Expression(..), nonTerminals, terminals, expression)
import Text.PrettyPrint.HughesPJ ((<+>), text, parens, hcat, Doc)
import Data.Foldable (Foldable (toList))
import Data.Maybe (catMaybes, isNothing, fromJust, fromMaybe, listToMaybe, mapMaybe)
import Data.Either (rights, lefts)
import Data.List (nub)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import Data.Bifunctor (Bifunctor(second, first))
import Control.Exception (Exception)
import Control.Applicative ((<|>))

data Pattern
    = PatEpsilon
    | PatT Terminal
    | PatNT NonTerminal Pattern
    | PatSeq Pattern Pattern
    | PatChoice Pattern Pattern
    | PatStar Pattern
    | PatNot Pattern
    | PatVar Symbol String
    deriving (Eq, Show, Ord)

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
    deriving (Eq, Show, Ord)

data Proof
    = ProofEpsilon
    | ProofT Terminal
    | ProofNT NonTerminal Proof
    | ProofSeq Proof Proof
    | ProofChoice Proof Proof
    | ProofChoiceLeft Proof
    | ProofChoiceRight Proof
    | ProofStar Proof
    | ProofNot Proof
    | ProofVar Symbol String
    deriving (Eq, Show, Ord)

type NamedPattern = (String, Pattern)
type NamedSynPat = (String, SyntaxPattern)

instance Pretty Pattern where
    pPrint :: Pattern -> Doc
    pPrint (PatNT nt p) = pPrint nt <+> text ":=" <+> (parens . pPrint) p
    pPrint (PatT t) = pPrint t
    pPrint (PatVar s name) = text ("#" ++ name) <> text ":" <> pPrint s
    pPrint PatEpsilon = text "ε"
    pPrint (PatSeq p1 p2) = pPrint p1 <+> pPrint p2
    pPrint (PatChoice p1 p2) = pPrint p1 <+> text "/" <+> pPrint p2
    pPrint (PatStar p) = (parens . pPrint) p <> text "*"
    pPrint (PatNot p) = text "!" <> (parens . pPrint) p

instance Pretty SyntaxPattern where
    pPrint :: SyntaxPattern -> Doc
    pPrint (SynNT nt ps) = pPrint nt <+> text ":=" <+> (parens . pPrint) ps
    pPrint (SynT t) = pPrint t
    pPrint (SynVar s name) = text ("#" ++ name) <> text ":" <> pPrint s
    pPrint SynEpsilon = text "ε"
    pPrint (SynSeq p1 p2) = pPrint p1 <+> pPrint p2
    pPrint (SynChoice p1 p2) = pPrint p1 <+> text "/" <+> pPrint p2
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
synToPat (SynSeq p1 p2) = PatSeq <$> synToPat p1 <*> synToPat p2
synToPat (SynChoice p1 p2) = PatChoice <$> synToPat p1 <*> synToPat p2
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
replaceInPat p (SynSeq p1 p2) = SynSeq (replaceInPat p p1) (replaceInPat p p2)
replaceInPat p (SynChoice p1 p2) = SynChoice (replaceInPat p p1) (replaceInPat p p2)
replaceInPat p (SynStar p') = SynStar $ replaceInPat p p'
replaceInPat p (SynNot p') = SynNot $ replaceInPat p p'
replaceInPat _ p = p

references :: SyntaxPattern -> [String]
references (SynNT _ ps) = references ps
references (SynRef s) = [s]
references (SynSeq p1 p2) = references p1 ++ references p2
references (SynChoice p1 p2) = references p1 ++ references p2
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

replaceSynPats :: [NamedSynPat] -> [NamedSynPat]
replaceSynPats ps = foldr replaceSynPat ps ps

replaceSynPat :: NamedSynPat -> [NamedSynPat] -> [NamedSynPat]
replaceSynPat p = map (second $ replaceInPat p)

-------------------------------------------------------------------------------
--- Validation

validPat' :: Grammar -> Pattern -> Maybe Proof
validPat' g (PatNT nt p) = ProofNT nt <$> (checkPat' g p =<< expr)
    where
        expr = expression g nt
validPat' g@(ds, _) p = firstJust (checkPat' g p . snd) ds
    where
        firstJust f = listToMaybe . mapMaybe f


checkPat' :: Grammar -> Pattern -> Expression -> Maybe Proof
checkPat' _ PatEpsilon           Empty            = Just ProofEpsilon
checkPat' _ (PatT t)             (ExprT t')       = toMaybe (t == t') (ProofT t)
checkPat' _ (PatVar (Right t) n) (ExprT t')       = toMaybe (t == t') (ProofVar (Right t) n)
checkPat' g p@(PatNT nt _)       (ExprNT nt')     = if nt == nt' then validPat' g p else Nothing
checkPat' _ (PatVar (Left nt) n) (ExprNT nt')     = toMaybe (nt == nt') (ProofVar (Left nt) n)
checkPat' g (PatSeq p1 p2)       (Sequence e1 e2) = ProofSeq <$> checkPat' g p1 e1 <*> checkPat' g p2 e2
checkPat' g (PatChoice p1 p2)    (Choice e1 e2)   = ProofChoice <$> checkPat' g p1 e1 <*> checkPat' g p2 e2
checkPat' g p                    (Choice e1 e2)   = (ProofChoiceLeft <$> checkPat' g p e1)
                                                    <|> (ProofChoiceRight <$> checkPat' g p e2)
checkPat' g (PatStar p)          (Star e)         = ProofStar <$> checkPat' g p e
checkPat' g (PatNot p)           (Not e)          = ProofNot <$> checkPat' g p e
checkPat' _ _                    _                = Nothing

correctPat :: Pattern -> Proof -> Maybe Pattern
correctPat PatEpsilon           ProofEpsilon             = Just PatEpsilon
correctPat (PatT t)             (ProofT t')              = toMaybe (t == t') (PatT t)
correctPat (PatNT nt pat)       (ProofNT nt' proof)      = if nt == nt' 
                                                            then PatNT nt <$> correctPat pat proof 
                                                            else Nothing
correctPat (PatVar (Left nt) n) (ProofVar (Left nt') n') = toMaybe (nt == nt' && n == n') (PatVar (Left nt) n)
correctPat (PatVar (Right t) n) (ProofVar (Right t') n') = toMaybe (t == t' && n == n') (PatVar (Right t) n)
correctPat (PatSeq p1 p2)       (ProofSeq p1' p2')       = PatSeq <$> correctPat p1 p1' <*> correctPat p2 p2'
correctPat (PatChoice p1 p2)    (ProofChoice p1' p2')    = PatChoice <$> correctPat p1 p1' <*> correctPat p2 p2'
correctPat pat                  (ProofChoiceLeft proof)  = flip PatChoice (PatNot PatEpsilon) <$> correctPat pat proof
correctPat pat                  (ProofChoiceRight proof) = PatChoice (PatNot PatEpsilon) <$> correctPat pat proof
correctPat (PatStar pat)        (ProofStar proof)        = PatStar <$> correctPat pat proof
correctPat (PatNot pat)         (ProofNot proof)         = PatNot <$> correctPat pat proof
correctPat _ _ = Nothing

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
checkPat g (PatSeq p1 p2)       (Sequence e1 e2) = checkPat g p1 e1 && checkPat g p2 e2
checkPat g (PatChoice p1 p2)    (Choice e1 e2)   = checkPat g p1 e1 && checkPat g p2 e2
checkPat g p                    (Choice e1 e2)   = checkPat g p e1 || checkPat g p e2
checkPat g (PatStar p)          (Star e)         = checkPat g p e
checkPat g (PatNot p)           (Not e)          = checkPat g p e
checkPat _ _                    _                = False

processSynPats :: Grammar -> [NamedSynPat] -> Either PatternException [NamedPattern]
processSynPats g ps =
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
                Right ps' -> processSynPats g $ replaceSynPats ps'