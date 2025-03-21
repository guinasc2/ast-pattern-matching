module Parser.ParsedTree where

import Syntax.Base (Pretty(pPrint), Terminal(..), NonTerminal(..))
import Syntax.Peg (Grammar, Expression(..), expression, processPeg)
import Syntax.Pattern
import Syntax.ParsedTree (ParsedTree(..), capture, match)
import Parser.Base (Parser, blank)
import Parser.Peg (grammar)
import Parser.Pattern (patterns)
import Text.Megaparsec
    (notFollowedBy, many, choice, parse, errorBundlePretty, eof, optional, try)
import Text.Megaparsec.Char (string)
import Data.Maybe (fromJust, catMaybes)

mkParser :: Grammar -> Parser ParsedTree
mkParser g@(_, nt) = ParsedNT nt <$> mkParser' g (fromJust $ Syntax.Peg.expression g nt)
                        <* optional blank 
                        <* eof

terminal :: Terminal -> Parser Terminal
terminal (T t) = T <$> string t

mkParser' :: Grammar -> Expression -> Parser ParsedTree
mkParser' _ Empty = ParsedEpsilon <$ string ""
mkParser' _ (ExprT t) = ParsedT <$> terminal t
mkParser' g (ExprNT nt) = ParsedNT nt <$> mkParser' g (fromJust $ Syntax.Peg.expression g nt)
-- mkParser' g (ExprNT nt) = if nt == NT "EOF"
--                             then ParsedNT (NT "EOF") ParsedEpsilon <$ eof
--                             else ParsedNT nt <$> mkParser' g (fromJust $ Syntax.Peg.expression g nt)
mkParser' g (Choice e1 e2) = choice [
                                try $ ParsedChoiceLeft <$> mkParser' g e1,
                                ParsedChoiceRight <$> mkParser' g e2
                            ]
mkParser' g (Sequence e1 e2) = ParsedSeq <$> mkParser' g e1 <*> mkParser' g e2
mkParser' g (Star e) = ParsedStar <$> many (mkParser' g e)
mkParser' g (Not e) = ParsedNot <$ notFollowedBy (mkParser' g e)

-------------------------------------------------------------------------------
--- Testes

parseApply :: FilePath -> FilePath -> IO ()
parseApply grammarFile inputFile = do
        contentsGrammar <- readFile grammarFile
        contentsInput <- readFile inputFile
        case parse grammar "" contentsGrammar of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right g ->
                case processPeg g of
                    Left bundle -> print bundle
                    Right g' -> 
                        case parse (mkParser g') "" contentsInput of
                            Left bundle -> putStr (errorBundlePretty bundle)
                            Right xs -> putStr $ show (pPrint xs)

parseApply' :: FilePath -> FilePath -> FilePath -> IO ()
parseApply' grammarFile patternFile inputFile = do
        contentsGrammar <- readFile grammarFile
        contentsPattern <- readFile patternFile
        contentsInput <- readFile inputFile
        case parse grammar "" contentsGrammar of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right g ->
                case parse patterns "" contentsPattern of
                    Left bundle -> putStr (errorBundlePretty bundle)
                    Right p ->
                        case processPats g p of
                            Left bundle -> print bundle
                            Right p' ->
                                case processPeg g of
                                    Left bundle -> print bundle
                                    Right g' -> 
                                        case parse (mkParser g') "" contentsInput of
                                            Left bundle -> putStr (errorBundlePretty bundle)
                                            Right xs -> do
                                                let ps' = map snd p'
                                                let proofs = map (validPat' g) ps'
                                                let corrected = catMaybes $ zipWith correctPat ps' (catMaybes proofs)
                                                let caps = map (`capture` xs) corrected
                                                let printCapture (v, t) = show (pPrint v) ++ "\n" ++ show (pPrint t) ++ "\n"
                                                print $ pPrint g
                                                print $ pPrint p'
                                                print $ pPrint xs
                                                putStrLn $ concatMap (concatMap printCapture) caps
                                                -- print p'
                                                -- print xs


