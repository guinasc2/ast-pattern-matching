module Main where

import Expr
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Test (quickCheck)


main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- Exp generator

genChar :: Gen Char
genChar = elements ['a'..'z']

genVar :: Gen Exp
genVar = 
    do
        namesize <- choose (1, 3)
        v <- vectorOf namesize genChar
        return (Var v)

genNum :: Gen Exp
genNum = 
    do
        n <- choose (1, 100)
        return (Num n)

genAdd :: Int -> Gen Exp
genAdd n =
    do
        e1 <- genExp (n-1)
        e2 <- genExp (n-1)
        return (Add e1 e2)

genMul :: Int -> Gen Exp
genMul n =
    do
        e1 <- genExp (n-1)
        e2 <- genExp (n-1)
        return (Mul e1 e2)

genExp :: Int -> Gen Exp
genExp 0 = frequency [(50, genNum), (50, genVar)]
genExp n = frequency [(25, genNum), (25, genVar), (25, genAdd n), (25, genMul n)]

genExp' :: Gen Exp
genExp' =
    do
        n <- choose (3, 7)
        genExp n


-- Pat generator


genPVar :: Gen Pat
genPVar = 
    do
        namesize <- choose (1, 3)
        v <- vectorOf namesize genChar
        return (PVar v)

genMetaVar :: Gen Pat
genMetaVar = 
    do
        namesize <- choose (1, 2)
        v <- vectorOf namesize genChar
        return (PVar ("#M" ++ v))

genPNum :: Gen Pat
genPNum = 
    do
        n <- choose (1, 100)
        return (PNum n)

genPAdd :: Int -> Gen Pat
genPAdd n =
    do
        e1 <- genPat (n-1)
        e2 <- genPat (n-1)
        return (PAdd e1 e2)

genPMul :: Int -> Gen Pat
genPMul n =
    do
        e1 <- genPat (n-1)
        e2 <- genPat (n-1)
        return (PMul e1 e2)

genPat :: Int -> Gen Pat
genPat 0 = frequency [(33, genPNum), (33, genPVar), (33, genMetaVar)]
genPat n = frequency [(25, genPNum), (25, genPVar), (25, genPAdd n), (25, genPMul n)]

genPat' :: Gen Pat
genPat' =
    do
        n <- choose (1, 5)
        genPat n