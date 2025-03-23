module Semantic.Pattern 
    ( PatternException(..)
    , Proof
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
import Data.Maybe (isNothing, fromJust, fromMaybe, listToMaybe, mapMaybe)
import Data.Either (rights, lefts)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import Data.Bifunctor (Bifunctor(second, first))
import Control.Exception (Exception)
import Control.Applicative ((<|>))
import Data.Data (Typeable, Data)
import Text.PrettyPrint.HughesPJ (Doc, text, (<+>), parens)
import Data.List (intercalate)

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
    deriving (Eq, Show, Ord, Typeable, Data)

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

instance Pretty PatternException where
    pPrint :: PatternException -> Doc
    pPrint (PatOutOfScope refs) = pPrint refs
    pPrint (PatRecursive pats) = text "As seguintes definições são mutuamente recursivas:"
                                 <+> pPrint pats
    pPrint (PatInvalid invalids) = pPrint invalids
    pPrint (PatDuplicate dups) = pPrint dups

instance Pretty RefOutOfScopeException where
    pPrint :: RefOutOfScopeException -> Doc
    pPrint (RefOutOfScope p refs) = text "O padrão" <+> pPrint p
                                    <+> text "depende de regras não definidas"
                                    <+> text (intercalate "," refs)

instance Pretty InvalidPatternException where
    pPrint :: InvalidPatternException -> Doc
    pPrint (InvalidPattern p) = text "Padrão inválido:" <+> pPrint p
    pPrint (InvalidSyntax p) = text "Sintaxe inválida:" <+> pPrint p

instance Pretty DuplicatePatternException where
    pPrint :: DuplicatePatternException -> Doc
    pPrint (DuplicatePattern n ps) = text "Múltiplas definições para " <+> text n
                                     <+> parens (pPrint ps)


-------------------------------------------------------------------------------

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

synToPat' :: NamedSynPat -> Either InvalidPatternException NamedPattern
synToPat' p@(n, sn) = maybeToRight (synToPat sn)
    where
        invalid = Left $ InvalidSyntax p
        maybeToRight = maybe invalid (Right . (n,))

dependencies ::
    NamedSynPat
    -> [NamedSynPat]
    -> Either RefOutOfScopeException [(NamedSynPat, NamedSynPat)] -- Lista de arestas
dependencies p ps =
    case filter (isNothing . snd) depends of
        [] -> Right $ map (\ x -> (second fromJust x, p)) depends
        x -> Left $ RefOutOfScope p (map fst x)
    where
        refs = references $ snd p
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
    case duplicatesOfFirst ps of
        [] -> Right ps
        ds -> Left . PatDuplicate $ map (\ x -> DuplicatePattern x (filterByFirst ps x)) ds

mkGraph :: [NamedSynPat] -> Either PatternException (Alga.AdjacencyMap NamedSynPat)
mkGraph ps =
    case duplicates ps of
        Left e -> Left e
        Right ps' -> second (Alga.overlay (Alga.vertices ps) . Alga.edges) (mkEdges ps')

topSort :: Alga.AdjacencyMap NamedSynPat -> Either PatternException [NamedSynPat]
topSort g = first (PatRecursive . toList) (Algo.topSort g)




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
checkPat' g p                    (Flatten e)      = checkPat' g p e
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
correctPat _                    _                        = Nothing

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
checkPat g p                    (Flatten e)      = checkPat g p e
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
