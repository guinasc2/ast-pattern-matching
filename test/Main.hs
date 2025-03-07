module Main where

import Syntax.Base
import Syntax.Peg
import Syntax.Pattern
import Syntax.ParsedTree
import Parser.Base
import Parser.Peg

import Text.Megaparsec (parse, Parsec)

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Test (quickCheck)
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $
        testGroup "Main tests" [testsParserPeg, testsMatch]

parseT :: Parsec e s a -> s -> a
parseT p f = case parse p "" f of
                    Right a -> a
                    Left _ -> error ""

testsParserPeg :: TestTree
testsParserPeg = testGroup "Tests Parser Peg"
    [
        testCase "peg simples, uma única regra" $
            parseT grammar "A <- \"a\"+"
            @?=
            ([(NT "A",Sequence (ExprT (T "a")) (Star (ExprT (T "a"))))],NT "A")

    ,   testCase "peg para expressões, com NT para número" $
            parseT grammar "E <- T (\"\\\"\" T)*\nT <- F (\"*\" F)*\nF <- \"num\" / \"(\" E \")\""
            @?=
            ([(NT "E",Sequence (ExprNT (NT "T")) (Star (Sequence (ExprT (T "\"")) (ExprNT (NT "T"))))),(NT "T",Sequence (ExprNT (NT "F")) (Star (Sequence (ExprT (T "*")) (ExprNT (NT "F"))))),(NT "F",Choice (ExprT (T "num")) (Sequence (ExprT (T "(")) (Sequence (ExprNT (NT "E")) (ExprT (T ")")))))],NT "E")

    ,   testCase "peg para expressões, com range para número" $
            parseT grammar "E <- T (\"+\" T)*\nT <- F (\"*\" F)*\nF <- [0-9]+ / \"(\" E \")\""
            @?=
            ([
                (NT "E",Sequence (ExprNT (NT "T")) (Star (Sequence (ExprT (T "+")) (ExprNT (NT "T"))))),
                (NT "T",Sequence (ExprNT (NT "F")) (Star (Sequence (ExprT (T "*")) (ExprNT (NT "F"))))),
                (NT "F",
                    Choice
                        (Sequence
                            (Choice (ExprT (T "0")) (Choice (ExprT (T "1")) (Choice (ExprT (T "2")) (Choice (ExprT (T "3")) (Choice (ExprT (T "4")) (Choice (ExprT (T "5")) (Choice (ExprT (T "6")) (Choice (ExprT (T "7")) (Choice (ExprT (T "8")) (ExprT (T "9")))))))))))
                            (Star (Choice (ExprT (T "0")) (Choice (ExprT (T "1")) (Choice (ExprT (T "2")) (Choice (ExprT (T "3")) (Choice (ExprT (T "4")) (Choice (ExprT (T "5")) (Choice (ExprT (T "6")) (Choice (ExprT (T "7")) (Choice (ExprT (T "8")) (ExprT (T "9")))))))))))))
                        (Sequence
                            (ExprT (T "("))
                            (Sequence (ExprNT (NT "E")) (ExprT (T ")")))))],NT "E")
    ]

testsMatch :: TestTree
testsMatch = testGroup "Tests Match"
    [
        testCase "Match Epsilon" $
            match PatEpsilon ParsedEpsilon @? ""

    ,   testCase "Match Nested Epsilon" $
            (not . match
                PatEpsilon)
                (ParsedSeq (ParsedEpsilon, ParsedT (T "teste"))) @? ""
    ,   testCase "Match expression tree" $
            match
                (PatSeq (PatT (T "1"),PatSeq (PatSeq (PatT (T "+"),PatVar (Left (NT "F")) "Teste"),PatSeq (PatT (T "+"),PatT (T "4")))))
                (ParsedSeq (ParsedT (T "1"),ParsedSeq (ParsedSeq (ParsedT (T "+"),ParsedNT (NT "F") (ParsedSeq (ParsedT (T "2"),ParsedSeq (ParsedT (T "*"),ParsedT (T "3"))))),ParsedSeq (ParsedT (T "+"),ParsedT (T "4")))))
            @? ""
    ,   testCase "Match with itself" $
            match
                (PatSeq    (PatT    (T "1"),PatSeq    (PatSeq    (PatT    (T "+"),PatNT    (NT "F") (PatSeq    (PatT    (T "2"),PatSeq    (PatT    (T "*"),PatT    (T "3"))))),PatSeq    (PatT    (T "+"),PatT    (T "4")))))
                (ParsedSeq (ParsedT (T "1"),ParsedSeq (ParsedSeq (ParsedT (T "+"),ParsedNT (NT "F") (ParsedSeq (ParsedT (T "2"),ParsedSeq (ParsedT (T "*"),ParsedT (T "3"))))),ParsedSeq (ParsedT (T "+"),ParsedT (T "4")))))
            @? ""
                
    ]