{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Syntax.Peg where

import Syntax.Base (NonTerminal(..), Terminal, Pretty(..))
import Text.PrettyPrint.HughesPJ (text, Doc, maybeParens, (<+>), (<>))
import Prelude hiding ((<>))
import Data.List (nub)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Data.Foldable (find, Foldable (toList))
import Data.Bifunctor (Bifunctor(first, second))
import Control.Exception (Exception)
import Data.Either (lefts, rights)
import qualified Algebra.Graph.AdjacencyMap as Alga
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo

data Expression
    = Empty
    | ExprT Terminal
    | ExprNT NonTerminal
    | Sequence Expression Expression
    | Choice Expression Expression
    | Star Expression
    | Not Expression
    deriving (Show, Eq, Ord)
{-
TODO: Fazer algo para eof e indentação
Rodrigo deu a ideia do eof ser o último símbolo da expressão inicial
    EOF ser o último símbolo é fácil de fazer
    Mas ao tentar gerar um parser de EOF com o megaparsec, 
        ele reclamou do tipo das coisas
        Outra opção pro EOF seria eu simplesmente inserir um eof depos da expressão inicial
        Assim, fica implícito que vai consumir tudo até o fim do arquivo
    Não tentei fazer de indentação, pq pareceu que tb daria problema de tipo
        ou precisaria de mais informação na PEG (por causa do tipo do parser de indentação)
-}
type Definition = (NonTerminal, Expression)

type Grammar = ([Definition], NonTerminal)

parensSeq :: Expression -> Bool
parensSeq (Choice _ _) = True
parensSeq _ = False

parensNot :: Expression -> Bool
parensNot (Choice _ _) = True
parensNot (Sequence _ _) = True
parensNot _ = False

parensStar :: Expression -> Bool
parensStar (Choice _ _) = True
parensStar (Sequence _ _) = True
parensStar (Not _) = True
parensStar _ = False

instance Pretty Expression where
    pPrint :: Expression -> Doc
    pPrint Empty = text "ε"
    pPrint (ExprT t) = pPrint t
    pPrint (ExprNT nt) = pPrint nt
    pPrint (Sequence e1 e2) = maybeParens (parensSeq e1) (pPrint e1)
                                <+> maybeParens (parensSeq e2) (pPrint e2)
    pPrint (Choice e1 e2) = pPrint e1 <+> text "/" <+> pPrint e2
    pPrint (Star e) = maybeParens (parensStar e) (pPrint e) <> text "*"
    pPrint (Not e) = text "!" <> maybeParens (parensNot e) (pPrint e)

instance Pretty Definition where
    pPrint :: Definition -> Doc
    pPrint (nt, e) = pPrint nt <+> text "<-" <+> pPrint e <> text "\n"

instance Pretty Grammar where
    pPrint :: Grammar -> Doc
    pPrint (ds, _) = pPrint ds


data RefOutOfScopeException
    = RefOutOfScope Definition [NonTerminal]
    deriving (Show, Eq, Ord)

data DuplicateDefinitionException
    = DuplicateDefinition NonTerminal [Expression]
    deriving (Show, Eq, Ord)

data PegException
    = PegOutOfScope [RefOutOfScopeException]
    | PegLeftRecursive [Definition]
    | PegStarNullable [Definition]
    | PegDuplicate [DuplicateDefinitionException]
    deriving (Show, Eq, Ord)

instance Exception RefOutOfScopeException
instance Exception DuplicateDefinitionException
instance Exception PegException

-------------------------------------------------------------------------------

nonTerminals :: Grammar -> [NonTerminal]
nonTerminals (ds, _) = map fst ds

terminals' :: Expression -> [Terminal]
terminals' Empty = []
terminals' (ExprT t) = [t]
terminals' (ExprNT _) = []
terminals' (Sequence e1 e2) = terminals' e1 ++ terminals' e2
terminals' (Choice e1 e2) = terminals' e1 ++ terminals' e2
terminals' (Star e) = terminals' e
terminals' (Not e) = terminals' e

terminals :: Grammar -> [Terminal]
terminals (ds, _) = nub $ concatMap (terminals' . snd) ds

expression :: Grammar -> NonTerminal -> Maybe Expression
expression (rs, _) nt = snd <$> find (\ x -> fst x == nt) rs

produces :: Grammar -> NonTerminal -> Terminal -> Bool
produces g nt t = isJust produce
    where
        expr = expression g nt
        rules = terminals' <$> expr
        produce = find (== t) =<< rules

processDot :: Grammar -> Grammar
processDot g = first (map processDef) g
    where
        t = foldr1 Choice $ map ExprT $ terminals g
        processDef = second (changeNT t)
        changeNT _ Empty = Empty
        changeNT _ e@(ExprT _) = e
        changeNT ts e@(ExprNT (NT nt)) = if nt == "" then ts else e
        changeNT ts (Sequence e1 e2) = Sequence (changeNT ts e1) (changeNT ts e2)
        changeNT ts (Choice e1 e2) = Choice (changeNT ts e1) (changeNT ts e2)
        changeNT ts (Star e) = Star (changeNT ts e)
        changeNT ts (Not e) = Not (changeNT ts e)

duplicates :: Grammar -> Either PegException Grammar
duplicates g@(defs, _) =
    case duplicates' defs [] [] of
        [] -> Right g
        ds -> Left . PegDuplicate $ map (\ x -> DuplicateDefinition x (filterEqual defs x)) ds
    where
        duplicates' [] _ dups = dups
        duplicates' (x:xs) checked dups =
            if fst x `elem` checked
                then duplicates' xs checked (fst x:dups)
                else duplicates' xs (fst x:checked) dups
        filterEqual g' x = map snd $ filter (\ p -> x == fst p) g'

nullable :: Grammar -> Definition -> Bool
nullable _ (_, Empty) = True
nullable _ (_, ExprT _) = False
-- nullable g (nt, ExprNT nt') = nt == nt' || nullable g (nt', fromJust $ expression g nt')
nullable g (nt, ExprNT nt') = nt == nt' || nullable g (nt', fromMaybe (error $ "Error " ++ show nt') (expression g nt'))
nullable g (nt, Sequence e1 e2) = nullable g (nt, e1) && nullable g (nt, e2)
nullable g (nt, Choice e1 e2) = nullable g (nt, e1) || nullable g (nt, e2)
nullable _ (_, Star _) = True
nullable _ (_, Not _) = True

starNullable :: Grammar -> Definition -> Maybe [Definition]
starNullable _ (_, Empty) = Nothing
starNullable _ (_, ExprT _) = Nothing
starNullable g (nt, ExprNT nt') = if nt == nt'
                                    then Just [(nt, ExprNT nt')]
                                    else starNullable g (nt', fromJust $ expression g nt')
starNullable g (nt, Sequence e1 e2) = case starNullable g (nt, e1) of
                                        Nothing -> Nothing
                                        Just _ -> starNullable g (nt, e2)
starNullable g (nt, Choice e1 e2) =
    f $
        fromMaybe [] (starNullable g (nt, e1))
        ++ fromMaybe [] (starNullable g (nt, e2))
    where
        f [] = Nothing
        f xs = Just xs
starNullable g (nt, Star e) = if nullable g (nt, e)
                                then Just [(nt, Star e)]
                                else starNullable g (nt, e)
starNullable g (nt, Not e) = starNullable g (nt, e)

recursiveLoop :: Grammar -> Maybe PegException
recursiveLoop g@(ds, _) =
    case filter isJust expr of
        [] -> Nothing
        xs -> Just $ PegLeftRecursive (concatMap fromJust xs)
    where
        expr = map (starNullable g) ds

referencesNull :: Grammar -> Definition -> [NonTerminal]
referencesNull _ (_, Empty) = []
referencesNull _ (_, ExprT _) = []
referencesNull _ (_, ExprNT nt') = [nt']
referencesNull g (nt, Sequence e1 e2) = if nullable g (nt, e1)
                                        then referencesNull g (nt, e1) ++ referencesNull g (nt, e2)
                                        else referencesNull g (nt, e1)
referencesNull g (nt, Choice e1 e2) = referencesNull g (nt, e1) ++ referencesNull g (nt, e2)
referencesNull g (nt, Star e) = referencesNull g (nt, e)
referencesNull g (nt, Not e) = referencesNull g (nt, e)

dependencies ::
    Grammar
    -> Definition
    -> Either RefOutOfScopeException [(Definition, Definition)] -- Lista de arestas
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
        p -> Left . PegOutOfScope $ p
    where
        result = map (dependencies g) ds

mkGraph :: Grammar -> Either PegException (Alga.AdjacencyMap Definition)
mkGraph g@(ds, _) =
    case duplicates g of
        Left e -> Left e
        Right ds' -> second (Alga.overlay (Alga.vertices ds) . Alga.edges) (mkEdges ds')

leftRecursive :: Alga.AdjacencyMap Definition -> Maybe PegException
leftRecursive g = leftToMaybe (first (PegLeftRecursive . toList) (Algo.topSort g))
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