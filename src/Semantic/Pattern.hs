{-|
Module      : Semantic.Pattern
Description : Semantic analysis of patterns in PEGs.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions for semantic analysis of patterns ('Pattern') in PEGs ('Grammar'). 
It includes validations, conversions, and error detection, such as invalid patterns, recursions, and duplications. 
It also defines specific exceptions for semantic errors related to patterns.
-}
module Semantic.Pattern
    ( PatternException(..)
    , Proof(..)
    , processPats
    , correctPat
    , validPat'
    ) where

import Syntax.Base (toMaybe, duplicatesOfFirst, filterByFirst, Terminal, NonTerminal, Symbol, Pretty(..))
import Syntax.Peg (Grammar, Expression(..), expression)
import Syntax.Pattern
    ( NamedSynPat
    , NamedPattern
    , SyntaxPattern(..)
    , Pattern(..)
    , references
    , replaceSynPats)
import Data.Foldable (Foldable(toList))
import Data.Maybe (isNothing, fromJust, fromMaybe, listToMaybe, mapMaybe, isJust)
import Data.Either (rights, lefts)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import Data.Bifunctor (Bifunctor(second, first))
import Control.Exception (Exception)
import Control.Applicative ((<|>))
import Data.Data (Typeable, Data)
import Text.PrettyPrint.HughesPJ (Doc, text, (<+>), parens)
import Data.List (intercalate)

-------------------------------------------------------------------------------
--- Types and Exceptions

{-|
Represents a proof of matching a pattern ('Pattern') with a PEG.

@since 1.0.0
-}
data Proof
    = ProofEpsilon
    | ProofT Terminal
    | ProofNT NonTerminal Proof
    | ProofSeq Proof Proof
    | ProofChoice Proof Proof
    | ProofChoiceLeft Proof
    | ProofChoiceRight Proof
    | ProofStar Proof
    | ProofStarSeq [Proof]
    | ProofNot Proof
    | ProofVar Symbol String
    deriving (Eq, Show, Ord, Typeable, Data)

{-|
Exception raised when a pattern references non-terminals out of scope.

@since 1.0.0
-}
data RefOutOfScopeException
    = RefOutOfScope NamedSynPat [String]
    deriving (Show, Eq, Ord)

{-|
Exception raised when a pattern is invalid.

@since 1.0.0
-}
data InvalidPatternException
    = InvalidSyntax NamedSynPat
    | InvalidPattern NamedPattern
    deriving (Show, Eq, Ord)

{-|
Exception raised when there are multiple definitions for the same pattern.

@since 1.0.0
-}
data DuplicatePatternException
    = DuplicatePattern String [SyntaxPattern]
    deriving (Show, Eq, Ord)

{-|
Exceptions related to the semantic analysis of patterns.

Possible exceptions include:
- 'PatOutOfScope': References to non-terminals out of scope.
- 'PatRecursive': Recursions between patterns.
- 'PatInvalid': Invalid patterns.
- 'PatDuplicate': Duplicate patterns.

@since 1.0.0
-}
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
--- Pretty Instances

{-|
Instance of the 'Pretty' class for 'PatternException'.

Prints the exception in a readable format, detailing the type of error and the elements involved.

@since 1.0.0
-}
instance Pretty PatternException where
    pPrint :: PatternException -> Doc
    pPrint (PatOutOfScope refs) = pPrint refs
    pPrint (PatRecursive pats) = text "The following definitions are mutually recursive:"
                                 <+> pPrint pats
    pPrint (PatInvalid invalids) = pPrint invalids
    pPrint (PatDuplicate dups) = pPrint dups

{-|
Instance of the 'Pretty' class for 'RefOutOfScopeException'.

Prints the exception indicating the pattern that references non-terminals out of scope.

@since 1.0.0
-}
instance Pretty RefOutOfScopeException where
    pPrint :: RefOutOfScopeException -> Doc
    pPrint (RefOutOfScope p refs) = text "The pattern" <+> pPrint p
                                    <+> text "depends on undefined rules"
                                    <+> text (intercalate "," refs)

{-|
Instance of the 'Pretty' class for 'InvalidPatternException'.

Prints the exception indicating the invalid pattern or invalid syntax.

@since 1.0.0
-}
instance Pretty InvalidPatternException where
    pPrint :: InvalidPatternException -> Doc
    pPrint (InvalidPattern p) = text "Invalid pattern:" <+> pPrint p
    pPrint (InvalidSyntax p) = text "Invalid syntax:" <+> pPrint p

{-|
Instance of the 'Pretty' class for 'DuplicatePatternException'.

Prints the exception indicating the duplicate pattern.

@since 1.0.0
-}
instance Pretty DuplicatePatternException where
    pPrint :: DuplicatePatternException -> Doc
    pPrint (DuplicatePattern n ps) = text "Multiple definitions for " <+> text n
                                     <+> parens (pPrint ps)

-------------------------------------------------------------------------------
--- Main Functions

{-|
Converts a 'SyntaxPattern' to a 'Pattern', if possible.

@since 1.0.0
-}
synToPat :: SyntaxPattern -> Maybe Pattern
synToPat SynEpsilon        = Just PatEpsilon
synToPat (SynT t)          = Just $ PatT t
synToPat (SynVar s n)      = Just $ PatVar s n
synToPat (SynRef _)        = Nothing
synToPat (SynNot p)        = PatNot <$> synToPat p
synToPat (SynNT nt p)      = PatNT nt <$> synToPat p
synToPat (SynSeq p1 p2)    = PatSeq <$> synToPat p1 <*> synToPat p2
synToPat (SynChoice p1 p2) = PatChoice <$> synToPat p1 <*> synToPat p2
synToPat (SynStar p)       = PatStar <$> synToPat p

{-|
Converts a 'SyntaxPattern' to a 'Pattern', if possible.

If the conversion fails (e.g., due to invalid references), returns an error.

@since 1.0.0
-}
synToPat' :: NamedSynPat -> Either InvalidPatternException NamedPattern
synToPat' p@(n, sn) = maybeToRight (synToPat sn)
    where
        invalid = Left $ InvalidSyntax p
        maybeToRight = maybe invalid (Right . (n,))

{-|
Calculates the dependencies of a syntactic pattern ('NamedSynPat').

Returns a list of edges representing the dependencies between patterns. If there are
references out of scope, returns an exception ('RefOutOfScopeException').

@since 1.0.0
-}
dependencies ::
    NamedSynPat
    -> [NamedSynPat]
    -> Either RefOutOfScopeException [(NamedSynPat, NamedSynPat)] -- List of edges
dependencies p ps =
    case filter (isNothing . snd) depends of
        [] -> Right $ map (\ x -> (second fromJust x, p)) depends
        x -> Left $ RefOutOfScope p (map fst x)
    where
        refs = references $ snd p
        findPats ps' s = (s, lookup s ps')
        depends = map (findPats ps) refs

{-|
Creates the edges of a dependency graph for syntactic patterns ('NamedSynPat').

If there are references out of scope, returns an exception ('PatOutOfScope').

@since 1.0.0
-}
mkEdges :: [NamedSynPat] -> Either PatternException [(NamedSynPat, NamedSynPat)]
mkEdges ps =
    case lefts result of
        [] -> Right . concat . rights $ result
        p -> Left . PatOutOfScope $ p
    where
        result = map (`dependencies` ps) ps

{-|
Checks for duplicate patterns in a list of syntactic patterns ('NamedSynPat').

If there are duplications, returns an exception ('PatDuplicate'). Otherwise, returns
the original list of patterns.

@since 1.0.0
-}
duplicates :: [NamedSynPat] -> Either PatternException [NamedSynPat]
duplicates ps =
    case duplicatesOfFirst ps of
        [] -> Right ps
        ds -> Left . PatDuplicate $ map (\ x -> DuplicatePattern x (filterByFirst ps x)) ds

{-|
Creates a dependency graph for syntactic patterns ('NamedSynPat').

If there are duplicate patterns or references out of scope, returns an exception.

@since 1.0.0
-}
mkGraph :: [NamedSynPat] -> Either PatternException (Alga.AdjacencyMap NamedSynPat)
mkGraph ps =
    case duplicates ps of
        Left e -> Left e
        Right ps' -> second (Alga.overlay (Alga.vertices ps) . Alga.edges) (mkEdges ps')

{-|
Performs topological sorting of a dependency graph of syntactic patterns.

If there are recursions, returns an exception ('PatRecursive'). Otherwise, returns
the sorted list of patterns.

@since 1.0.0
-}
topSort :: Alga.AdjacencyMap NamedSynPat -> Either PatternException [NamedSynPat]
topSort g = first (PatRecursive . toList) (Algo.topSort g)

{-|
Validates a pattern ('Pattern') against a PEG ('Grammar').
Returns a proof if the pattern is valid.

@since 1.0.0
-}
validPat' :: Grammar -> Pattern -> Maybe Proof
validPat' g (PatNT nt p) = ProofNT nt <$> (checkPat' g p =<< expr)
    where
        expr = expression g nt
validPat' g@(ds, _) p = firstJust (checkPat' g p . snd) ds
    where
        firstJust f = listToMaybe . mapMaybe f

{-|
Checks if a pattern ('Pattern') matches an expression ('Expression').
Returns a proof if the pattern is valid.

@since 1.0.0
-}
checkPat' :: Grammar -> Pattern -> Expression -> Maybe Proof
checkPat' _ PatEpsilon           Empty            = Just ProofEpsilon
checkPat' _ (PatT t)             (ExprT t')       = toMaybe (t == t') (ProofT t)
checkPat' _ (PatVar (Right t) n) (ExprT t')       = toMaybe (t == t') (ProofVar (Right t) n)
checkPat' g p@(PatNT nt _)       (ExprNT nt')     = if nt == nt' then validPat' g p else Nothing
checkPat' _ (PatVar (Left nt) n) (ExprNT nt')     = toMaybe (nt == nt') (ProofVar (Left nt) n)
checkPat' g (PatSeq p1 p2)       (Sequence e1 e2) = ProofSeq <$> checkPat' g p1 e1 <*> checkPat' g p2 e2
checkPat' g (PatSeq p1 p2)       (Indent e1 e2)   = ProofSeq <$> checkPat' g p1 e1 <*> checkPat' g p2 e2
checkPat' g p@(PatSeq _  _)      (Star e)         = ProofStarSeq <$> checkPatStar' g p e
checkPat' g (PatChoice p1 p2)    (Choice e1 e2)   = ProofChoice <$> checkPat' g p1 e1 <*> checkPat' g p2 e2
checkPat' g p                    (Choice e1 e2)   = (ProofChoiceLeft <$> checkPat' g p e1)
                                                    <|> (ProofChoiceRight <$> checkPat' g p e2)
checkPat' g (PatStar p)          (Star e)         = ProofStar <$> checkPat' g p e
checkPat' g (PatNot p)           (Not e)          = ProofNot <$> checkPat' g p e
checkPat' _ (PatT t)             (Flatten _)      = Just $ ProofT t
checkPat' g p                    (Flatten e)      = checkPat' g p e
checkPat' _ _                    _                = Nothing

checkPatStar' :: Grammar -> Pattern -> Expression -> Maybe [Proof]
checkPatStar' g p@(PatSeq p1 p2) e = if isJust p1' then (:) <$> p1' <*> p2' else (:[]) <$> checkPat' g p e
    where
        p1' = checkPat' g p1 e
        p2' = checkPatStar' g p2 e
checkPatStar' g p                e = (:[]) <$> checkPat' g p e

{-|
Corrects a pattern ('Pattern') based on a proof of matching ('Proof').

@since 1.0.0
-}
correctPat :: Pattern -> Proof -> Maybe Pattern
correctPat PatEpsilon           ProofEpsilon             = Just PatEpsilon
correctPat (PatT t)             (ProofT t')              = toMaybe (t == t') (PatT t)
correctPat (PatNT nt pat)       (ProofNT nt' proof)      = if nt == nt'
                                                            then PatNT nt <$> correctPat pat proof
                                                            else Nothing
correctPat (PatVar (Left nt) n) (ProofVar (Left nt') n') = toMaybe (nt == nt' && n == n') (PatVar (Left nt) n)
correctPat (PatVar (Right t) n) (ProofVar (Right t') n') = toMaybe (t == t' && n == n') (PatVar (Right t) n)
correctPat (PatSeq p1 p2)       (ProofSeq p1' p2')       = PatSeq <$> correctPat p1 p1' <*> correctPat p2 p2'
correctPat p@(PatSeq _ _)       (ProofStarSeq xs)        = PatStarSeq <$> correctPatStar p xs
correctPat (PatChoice p1 p2)    (ProofChoice p1' p2')    = PatChoice <$> correctPat p1 p1' <*> correctPat p2 p2'
correctPat pat                  (ProofChoiceLeft proof)  = flip PatChoice (PatNot PatEpsilon) <$> correctPat pat proof
correctPat pat                  (ProofChoiceRight proof) = PatChoice (PatNot PatEpsilon) <$> correctPat pat proof
correctPat (PatStar pat)        (ProofStar proof)        = PatStar <$> correctPat pat proof
correctPat (PatNot pat)         (ProofNot proof)         = PatNot <$> correctPat pat proof
correctPat _                    _                        = Nothing

correctPatStar :: Pattern -> [Proof] -> Maybe [Pattern]
correctPatStar _ [] = Nothing
correctPatStar p [x] = (:[]) <$> correctPat p x
correctPatStar (PatSeq p1 p2) (x:xs) = (:) <$> correctPat p1 x <*> correctPatStar p2 xs
correctPatStar _ (_:_) = Nothing

{-|
Validates a pattern ('Pattern') against a PEG ('Grammar').

Returns `True` if the pattern is valid, or `False` otherwise.

@since 1.0.0
-}
validPat :: Grammar -> Pattern -> Bool
validPat g (PatNT nt p) = fromMaybe False rule
    where
        expr = expression g nt
        rule = checkPat g p <$> expr
validPat g@(ds, _) p = any (checkPat g p . snd) ds

{-|
Checks if a pattern ('Pattern') matches an expression ('Expression').

Returns `True` if there is a match, or `False` otherwise.

@since 1.0.0
-}
checkPat :: Grammar -> Pattern -> Expression -> Bool
checkPat _ PatEpsilon           Empty            = True
checkPat _ (PatT t)             (ExprT t')       = t == t'
checkPat _ (PatVar (Right t) _) (ExprT t')       = t == t'
checkPat g p@(PatNT nt _)       (ExprNT nt')     = nt == nt' && validPat g p
checkPat _ (PatVar (Left nt) _) (ExprNT nt')     = nt == nt'
checkPat g (PatSeq p1 p2)       (Sequence e1 e2) = checkPat g p1 e1 && checkPat g p2 e2
checkPat g (PatSeq p1 p2)       (Indent e1 e2)   = checkPat g p1 e1 && checkPat g p2 e2
checkPat g p@(PatSeq _ _)       (Star e)         = checkPatStar g p e
checkPat g (PatChoice p1 p2)    (Choice e1 e2)   = checkPat g p1 e1 && checkPat g p2 e2
checkPat g p                    (Choice e1 e2)   = checkPat g p e1 || checkPat g p e2
checkPat g (PatStar p)          (Star e)         = checkPat g p e
checkPat g (PatNot p)           (Not e)          = checkPat g p e
checkPat _ (PatT _)             (Flatten _)      = True
checkPat g p                    (Flatten e)      = checkPat g p e
checkPat _ _                    _                = False

checkPatStar :: Grammar -> Pattern -> Expression -> Bool
checkPatStar g p@(PatSeq p1 p2) e = if checkPat g p1 e then checkPatStar g p2 e else checkPat g p e
checkPatStar g p                e = checkPat g p e

{-|
Processes a list of syntactic patterns ('NamedSynPat') and validates the patterns.

Returns a list of valid patterns or an exception indicating the errors found.

@since 1.0.0
-}
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

{-|
Processes a list of syntactic patterns ('NamedSynPat') and validates the patterns.

Returns a list of valid patterns or an exception indicating the errors found.

@since 1.0.0
-}
processPats :: Grammar -> [NamedSynPat] -> Either PatternException [NamedPattern]
processPats g ps =
    case mkGraph ps of
        Left x -> Left x
        Right graph ->
            case topSort graph of
                Left x -> Left x
                Right ps' -> processSynPats g $ replaceSynPats ps'
