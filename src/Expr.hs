module Expr where

import Data.Maybe

data Exp
    = Var String
    | Num Int
    | Add Exp Exp
    | Mul Exp Exp
    deriving (Eq, Show)

data Pat
    = MetaVar String
    | AnyVar String
    | AnyNum String
    | PVar String
    | PNum Int
    | PAdd Pat Pat
    | PMul Pat Pat
    deriving (Eq, Show)

data Comb
    = Single Pat
    | And Comb Comb
    | Or Comb Comb
    | Not Comb
    deriving (Eq, Show)

-- instance Show Exp where
--     show (Var v) = v
--     show (Num n) = show n
--     show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
--     show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

-- instance Show Pat where
--     show (MetaVar v) = v
--     show (AnyVar v) = v
--     show (AnyNum v) = v
--     show (PVar v) = v
--     show (PNum n) = show n
--     show (PAdd e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
--     show (PMul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

-- instance Show Comb where
--     show (Single p) = show p
--     show (And c1 c2) = "(" ++ show c1 ++ " && " ++ show c2 ++ ")"
--     show (Or c1 c2) = "(" ++ show c1 ++ " || " ++ show c2 ++ ")"
--     show (Not c) = "~(" ++ show c ++ ")"


matchTree :: Pat -> Exp -> Maybe [(String, Exp)]
matchTree (MetaVar v) t  = pure [(v, t)]
matchTree (AnyVar v) e@(Var _) = pure [(v, e)]
matchTree (AnyNum v) e@(Num _) = pure [(v, e)]
matchTree (PVar a) (Var b) = if a == b then pure [] else Nothing
matchTree (PNum n) (Num m) = if n == m then pure [] else Nothing
matchTree (PAdd p1 p2) (Add e1 e2) = 
    case (matchTree p1 e1, matchTree p2 e2) of
        (Just x, Just y) -> Just (x ++ y)
        _                -> Nothing
matchTree (PMul p1 p2) (Mul e1 e2) = 
    case (matchTree p1 e1, matchTree p2 e2) of
        (Just x, Just y) -> Just (x ++ y)
        _                -> Nothing
matchTree _ _ = Nothing


-- matchTest  "a * (#M1 + 2)" "a * c * (b + 2)"
deepMatchTree :: Pat -> Exp -> Maybe [(String, Exp)]
deepMatchTree p e = 
    case matchTree p e of
        Just x -> Just x
        Nothing ->
            case e of
                (Add e1 e2) ->
                    case (deepMatchTree p e1) of
                        Just x -> Just x
                        Nothing -> deepMatchTree p e2
                (Mul e1 e2) ->
                    case (deepMatchTree p e1) of
                        Just x -> Just x
                        Nothing -> deepMatchTree p e2
                _ -> Nothing


matchComb :: Comb -> Exp -> Bool
matchComb (Single p) e = isJust $ deepMatchTree p e
matchComb (And c1 c2) e = matchComb c1 e && matchComb c2 e
matchComb (Or c1 c2) e = matchComb c1 e || matchComb c2 e
matchComb (Not c) e = not $ matchComb c e
