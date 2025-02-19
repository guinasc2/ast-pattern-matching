{-# LANGUAGE TupleSections #-}

module Syntax.Pattern where

import Syntax.Base
import Data.Foldable (Foldable (toList))
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Either (rights, lefts)
import Data.List (nub)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import Data.Bifunctor (Bifunctor(second, first))
import Control.Exception (Exception)


data Pat
    = PatNT NonTerminal [Pat]
    | PatT Terminal
    | PatVar Symbol String
    deriving (Eq, Show, Ord)

data SyntaxPat
    = SynNT NonTerminal [SyntaxPat]
    | SynT Terminal
    | SynVar Symbol String
    | SynRef String
    deriving (Eq, Show, Ord)

type NamedPat = (String, Pat)
type NamedSynPat = (String, SyntaxPat)

data RefOutOfScopeException
    = RefOutOfScope NamedSynPat [String]
    deriving (Show, Eq, Ord)

data InvalidPatternException
    = InvalidSyntax NamedSynPat
    | InvalidPattern NamedPat
    deriving (Show, Eq, Ord)

data DuplicatePatternException
    = DuplicatePattern String [SyntaxPat]
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

synToPat :: SyntaxPat -> Maybe Pat
synToPat (SynNT nt ps) = if hasNothing then Nothing else Just $ PatNT nt (catMaybes ps')
    where
        ps' = map synToPat ps
        hasNothing = Nothing `elem` ps'
synToPat (SynT t) = Just $ PatT t
synToPat (SynVar s n) = Just $ PatVar s n
synToPat (SynRef _) = Nothing

synToPat' :: NamedSynPat -> Either InvalidPatternException NamedPat
synToPat' p@(n, sn) = maybe invalid (Right . (n,)) (synToPat sn)
    where
        invalid = Left $ InvalidSyntax p

replaceInPat :: NamedSynPat -> SyntaxPat -> SyntaxPat
replaceInPat (n, p) (SynRef n') = if n == n' then p else SynRef n'
replaceInPat p (SynNT nt ps) = SynNT nt $ map (replaceInPat p) ps
replaceInPat _ p = p

references :: SyntaxPat -> [String]
references (SynNT _ ps) = concatMap references ps
references (SynRef s) = [s]
references _ = []

dependencies ::
    NamedSynPat
    -> [NamedSynPat]
    -> Either RefOutOfScopeException [(NamedSynPat, NamedSynPat)]
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
        ds -> Left . PatDuplicate $ map (\ x -> DuplicatePattern x (find' ps x)) ds
    where
        duplicates' [] _ dups = dups
        duplicates' (x:xs) checked dups =
            if fst x `elem` checked
                then duplicates' xs checked (fst x:dups)
                else duplicates' xs (fst x:checked) dups
        -- find' ps' x = map snd $ filter (\ p -> x == fst p) ps' 
        find' [] _ = []
        find' (p:ps') x = if x == fst p then snd p : find' ps' x else find' ps' x

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
-- Prints 
-- TODO: Trocar para biblioteca printy

showP :: Pat -> String
showP (PatNT (NT nt) ps) = nt ++ " := (" ++ concatMap showP ps ++ ") "
showP (PatT (T t)) = "\"" ++ t ++ "\" "
showP (PatVar (Left (NT nt)) n) = n ++ ":" ++ nt ++ " "
showP (PatVar (Right (T t)) n) = n ++ ":" ++ "\"" ++ t ++ "\" "

showP' :: SyntaxPat -> String
showP' (SynNT (NT nt) ps) = nt ++ " := (" ++ concatMap showP' ps ++ ") "
showP' (SynT (T t)) = "\"" ++ t ++ "\" "
showP' (SynVar (Left (NT nt)) n) = n ++ ":" ++ nt ++ " "
showP' (SynVar (Right (T t)) n) = n ++ ":" ++ "\"" ++ t ++ "\" "
showP' (SynRef n) = "@" ++ n ++ " "
