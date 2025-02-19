module Syntax.Grammar where

import Syntax.Base
import Syntax.Pattern
import Data.Either (rights, lefts)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Foldable (find)

type RHS = [Symbol]
type Rule = (NonTerminal, RHS)
type Grammar = ([Rule], NonTerminal)


-------------------------------------------------------------------------------
--- TODO: Colocar as que dependem de Pattern em um arquivo de semânticas?

validPat :: Grammar -> Pat -> Bool
validPat g (PatNT nt ps) = isJust rule
    where
        rules = nonTerminalRules g nt
        rhs = map snd rules
        rule = find (\ x -> checkPat g x ps) rhs
validPat g (PatT t) = t `elem` terminals g
validPat g (PatVar s _) =
    case s of
        Left nt -> nt `elem` nonTerminals g
        Right t -> t `elem` terminals g

nonTerminals :: Grammar -> [NonTerminal]
nonTerminals (rs, _) = nub $ map fst rs

terminals :: Grammar -> [Terminal]
terminals (rs, _) = rights $ concatMap snd rs

nonTerminalRules :: Grammar -> NonTerminal -> [Rule]
nonTerminalRules (rs, _) nt = filter (\ x -> fst x == nt) rs

checkPat :: Grammar -> RHS -> [Pat] -> Bool
checkPat _ [] [] = True
checkPat _ [] _ = False
checkPat _ _ [] = False
checkPat g (r:rs) (p:ps) =
    case (r, p) of
        (Left nt, PatNT nt' _) -> nt == nt' && validPat g p && checkPat g rs ps
        (Left nt, PatVar (Left nt') _) -> nt == nt' && checkPat g rs ps
        (Right t, PatT t') -> t == t' && checkPat g rs ps
        (Right t, PatVar (Right t') _) -> t == t' && checkPat g rs ps

        (Left nt, PatT t) -> produces g nt t && checkPat g rs ps
        (Left nt, PatVar (Right t) _) -> produces g nt t && checkPat g rs ps
        _ -> False

-- Não checa transitividade
produces :: Grammar -> NonTerminal -> Terminal -> Bool
produces g nt t = isJust produce
    where
        rules = nonTerminalRules g nt
        rhs = filter (\ x -> length x == 1) $ map snd rules
        produce = find (== t) (rights . concat $ rhs)


validSynPats :: Grammar -> [NamedSynPat] -> Either PatternException [NamedPat]
validSynPats g ps =
    case lefts pats of
        [] -> case filter (not . validPat g . snd) valids of
                [] -> Right valids
                invalids -> Left $ PatInvalid (map InvalidPattern invalids)
        e -> Left $ PatInvalid e
    where
        pats = map synToPat' ps
        valids = rights pats
        

processPats :: Grammar -> [NamedSynPat] -> Either PatternException [NamedPat]
processPats g ps =
    case mkGraph ps of
        Left x -> Left x
        Right graph ->
            case topSort graph of
                Left x -> Left x
                Right ps' -> validSynPats g $ replacePats ps'

-------------------------------------------------------------------------------

showG :: Grammar -> String
showG (r, _) = concatMap showR r

showR :: Rule -> String
showR (NT nt, rhs) = nt ++ " -> " ++ showRhs rhs ++ "\n"

showRhs :: RHS -> String
showRhs = concatMap showS