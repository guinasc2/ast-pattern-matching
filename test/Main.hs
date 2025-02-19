module Main where

import Syntax.Base
import Syntax.Peg
import Parser.Base
import Parser.Peg

import Text.Megaparsec

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Test (quickCheck)
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests

parseT :: Parsec e s a -> s -> a
parseT p f = case parse p "" f of
                    Right a -> a
                    Left _ -> error ""

tests :: TestTree
tests = testGroup "Tests"
  [ 
    testCase "peg simples, uma única regra" $
        parseT grammar "A <- \"a\"+" 
        @?= 
        ([(NT "A",Sequence (ExprT (T "a")) (Star (ExprT (T "a"))))],NT "A")
  
  , testCase "peg para expressões, com NT para número" $
        parseT grammar "E <- T (\"\\\"\" T)*\nT <- F (\"*\" F)*\nF <- \"num\" / \"(\" E \")\""
        @?=
        ([(NT "E",Sequence (ExprNT (NT "T")) (Star (Sequence (ExprT (T "\"")) (ExprNT (NT "T"))))),(NT "T",Sequence (ExprNT (NT "F")) (Star (Sequence (ExprT (T "*")) (ExprNT (NT "F"))))),(NT "F",Choice (ExprT (T "num")) (Sequence (Sequence (ExprT (T "(")) (ExprNT (NT "E"))) (ExprT (T ")"))))],NT "E")
      
    , testCase "peg para expressões, com range para número" $
        parseT grammar "E <- T (\"+\" T)*\nT <- F (\"*\" F)*\nF <- [0-9]+ / \"(\" E \")\"" 
        @?= 
        ([
            (NT "E",Sequence (ExprNT (NT "T")) (Star (Sequence (ExprT (T "+")) (ExprNT (NT "T"))))),
            (NT "T",Sequence (ExprNT (NT "F")) (Star (Sequence (ExprT (T "*")) (ExprNT (NT "F"))))),
            (NT "F",
                Choice 
                    (Sequence 
                        (Choice (Choice (Choice (Choice (Choice (Choice (Choice (Choice (Choice (ExprT (T "0")) (ExprT (T "1"))) (ExprT (T "2"))) (ExprT (T "3"))) (ExprT (T "4"))) (ExprT (T "5"))) (ExprT (T "6"))) (ExprT (T "7"))) (ExprT (T "8"))) (ExprT (T "9")))
                        (Star (Choice (Choice (Choice (Choice (Choice (Choice (Choice (Choice (Choice (ExprT (T "0")) (ExprT (T "1"))) (ExprT (T "2"))) (ExprT (T "3"))) (ExprT (T "4"))) (ExprT (T "5"))) (ExprT (T "6"))) (ExprT (T "7"))) (ExprT (T "8"))) (ExprT (T "9")))))
                    (Sequence 
                        (Sequence (ExprT (T "(")) (ExprNT (NT "E"))) 
                        (ExprT (T ")"))))],NT "E")
  ]