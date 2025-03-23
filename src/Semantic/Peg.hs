module Semantic.Peg 
    ( PegException(..)
    , processPeg
    ) where

import Syntax.Base (NonTerminal(..), duplicatesOfFirst, filterByFirst, Pretty(..))
import Syntax.Peg (Expression(..), Definition, Grammar, terminals, expression)
import Data.List (nub)
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Foldable (Foldable (toList))
import Data.Bifunctor (Bifunctor(first, second))
import Control.Exception (Exception)
import Data.Either (lefts, rights)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import Data.Generics (mkT, everywhere)
import Text.PrettyPrint.HughesPJ (Doc, text, (<+>), parens)


data RefOutOfScopeException
    = RefOutOfScope Definition [NonTerminal]
    deriving (Show, Eq, Ord)

data DuplicateDefinitionException
    = DuplicateDefinition NonTerminal [Expression]
    deriving (Show, Eq, Ord)

data PegException
    = OutOfScope [RefOutOfScopeException]
    | LeftRecursive [Definition]
    | StarNullable [Definition]
    | DuplicateRule [DuplicateDefinitionException]
    deriving (Show, Eq, Ord)

instance Exception RefOutOfScopeException
instance Exception DuplicateDefinitionException
instance Exception PegException

instance Pretty PegException where
    pPrint :: PegException -> Doc
    pPrint (OutOfScope refs) = pPrint refs
    pPrint (LeftRecursive defs) = text "As seguintes definições causam uma recursão a esquerda:"
                                    <+> pPrint defs
    pPrint (StarNullable defs) = text "As seguintes definições possuem expressões anuláveis dentro do *:"
                                    <+> pPrint defs
    pPrint (DuplicateRule defs) = pPrint defs

instance Pretty RefOutOfScopeException where
    pPrint :: RefOutOfScopeException -> Doc
    pPrint (RefOutOfScope def nts) = text "A regra" <+> pPrint def 
                                     <+> text "depende de regras não definidas"
                                     <+> parens (pPrint nts)

instance Pretty DuplicateDefinitionException where
    pPrint :: DuplicateDefinitionException -> Doc
    pPrint (DuplicateDefinition nt es) = text "Múltiplas definições para " <+> pPrint nt
                                         <+> parens (pPrint es)

-------------------------------------------------------------------------------

processDot :: Grammar -> Grammar
processDot g = first (map processDef) g
    where
        t = foldr1 Choice $ map ExprT $ terminals g
        processDef = second (everywhere $ mkT (changeNT t))
        changeNT ts e@(ExprNT (NT nt)) = if nt == "." then ts else e
        changeNT _  e                  = e

duplicates :: Grammar -> Either PegException Grammar
duplicates g@(defs, _) =
    case duplicatesOfFirst defs of
        [] -> Right g
        ds -> Left . DuplicateRule $ map (\ x -> DuplicateDefinition x (filterByFirst defs x)) ds

nullable :: Grammar -> Definition -> Bool
nullable _ (_, Empty)           = True
nullable _ (_, Star _)          = True
nullable _ (_, Not _)           = True
nullable _ (_, ExprT _)         = False
nullable g (nt, ExprNT nt')     = nt == nt' 
                                  || nullable g (nt', fromMaybe (error $ "Error " ++ show nt') (expression g nt'))
nullable g (nt, Sequence e1 e2) = nullable g (nt, e1) && nullable g (nt, e2)
nullable g (nt, Choice e1 e2)   = nullable g (nt, e1) || nullable g (nt, e2)
nullable g (nt, Flatten e)      = nullable g (nt, e)

starNullable :: Grammar -> Definition -> [Definition]
starNullable _ (_, Empty)           = []
starNullable _ (_, ExprT _)         = []
starNullable g (nt, Not e)          = starNullable g (nt, e)
starNullable g (nt, Flatten e)      = starNullable g (nt, e)
starNullable g (nt, Choice e1 e2)   = starNullable g (nt, e1) ++ starNullable g (nt, e2)
starNullable g (nt, ExprNT nt')     = if nt == nt'
                                        then [(nt, ExprNT nt')]
                                        else starNullable g (nt', fromJust $ expression g nt')
starNullable g (nt, Sequence e1 e2) = if nullable g (nt, e1)
                                        then starNullable g (nt, e1) ++ starNullable g (nt, e2)
                                        else starNullable g (nt, e1)
starNullable g (nt, Star e)         = if nullable g (nt, e)
                                        then [(nt, Star e)]
                                        else starNullable g (nt, e)

recursiveLoop :: Grammar -> Maybe PegException
recursiveLoop g@(ds, _) =
    case expr of
        [] -> Nothing
        xs -> Just $ StarNullable xs
    where
        expr = nub $ concatMap (starNullable g) ds

referencesNull :: Grammar -> Definition -> [NonTerminal]
referencesNull _ (_, Empty)           = []
referencesNull _ (_, ExprT _)         = []
referencesNull _ (_, ExprNT nt')      = [nt']
referencesNull g (nt, Sequence e1 e2) = if nullable g (nt, e1)
                                        then referencesNull g (nt, e1) ++ referencesNull g (nt, e2)
                                        else referencesNull g (nt, e1)
referencesNull g (nt, Choice e1 e2)   = referencesNull g (nt, e1) ++ referencesNull g (nt, e2)
referencesNull g (nt, Star e)         = referencesNull g (nt, e)
referencesNull g (nt, Not e)          = referencesNull g (nt, e)
referencesNull g (nt, Flatten e)      = referencesNull g (nt, e)

dependencies ::
    Grammar
    -> Definition
    -> Either RefOutOfScopeException [(Definition, Definition)] -- Lista de dependências
dependencies g@(ds, _) d =
    case filter (isNothing . snd) depends of
        [] -> Right $ map (\ x -> (second fromJust x, d)) depends
        x -> Left $ RefOutOfScope d (map fst x)
    where
        refs = nub $ referencesNull g d
        findNTs ds' s = (s, lookup s ds')
        depends = map (findNTs ds) refs

mkEdges :: Grammar -> Either PegException [(Definition, Definition)]
mkEdges g@(ds, _) =
    case lefts result of
        [] -> Right . concat . rights $ result
        p -> Left . OutOfScope $ p
    where
        result = map (dependencies g) ds

mkGraph :: Grammar -> Either PegException (Alga.AdjacencyMap Definition)
mkGraph g@(ds, _) =
    case duplicates g of
        Left e -> Left e
        Right ds' -> second (Alga.overlay (Alga.vertices ds) . Alga.edges) (mkEdges ds')

leftRecursive :: Alga.AdjacencyMap Definition -> Maybe PegException
leftRecursive g = leftToMaybe (first (LeftRecursive . toList) (Algo.topSort g))
    where
        leftToMaybe = either Just (const Nothing)

processPeg :: Grammar -> Either PegException Grammar
processPeg g =
    case mkGraph g' of
        Left x -> Left x
        Right graph ->
            case leftRecursive graph of
                Just x -> Left x
                Nothing -> maybe (Right g') Left (recursiveLoop g')
    where
        g' = processDot g
