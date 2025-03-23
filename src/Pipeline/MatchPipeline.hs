module Pipeline.MatchPipeline where

import Syntax.Base (Pretty(pPrint))
import Syntax.Peg (Grammar)
import Syntax.Pattern (NamedSynPat, NamedPattern, Pattern)
import Syntax.ParsedTree (ParsedTree, flatten)
import Parser.Base (parseWith)
import Parser.Peg (parseGrammar)
import Parser.Pattern (parsePatterns)
import Parser.ParsedTree (mkParser)
import Semantic.Peg (processPeg)
import Semantic.Pattern (validPat', correctPat, processPats)
import Match.Capture (match, capture)
import Match.Rewrite (rewrite)
import Text.Megaparsec (errorBundlePretty)
import Data.Bifunctor (Bifunctor(first, bimap))
import Data.Foldable (find)

type PrettyError = String


parseValidGrammar :: String -> Either PrettyError Grammar
parseValidGrammar contents =
    case parseGrammar contents of
        Left e -> Left $ errorBundlePretty e
        Right g -> first (show . pPrint) (processPeg g)

parseValidPatterns :: String -> String -> Either PrettyError [NamedPattern]
parseValidPatterns contentsG contentsP =
    case (g, ps) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right g', Right ps') -> first show (processPats g' ps')
    where
        g = parseValidGrammar contentsG
        ps = first errorBundlePretty (parsePatterns contentsP)

parseCorrectPatterns :: String -> String -> Either PrettyError [NamedPattern]
parseCorrectPatterns contentsG contentsP =
    case (g, ps) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right g', Right ps') -> bimap show (correct g') (processPats g' ps')
    where
        g = parseValidGrammar contentsG
        ps = first errorBundlePretty (parsePatterns contentsP)
        mkProof g' (n, p) ps' = maybe ps' ((:ps') . (n,)) (correctPat p =<< validPat' g' p)
        correct g' = foldr (mkProof g') []


parseFile :: String -> String -> Either PrettyError ParsedTree
parseFile contentsG contentsF =
    case parseValidGrammar contentsG of
        Left e -> Left e
        Right g -> first errorBundlePretty (pFile g)
    where
        pFile g = parseWith (mkParser g) contentsF

parseMatch :: String -> String -> String -> Either PrettyError [(String, Bool)]
parseMatch contentsG contentsP contentsF =
    case (ps, f) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right ps', Right f') -> Right $ map (match' f') ps'
    where
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF
        match' f' (n, p) = (n, Match.Capture.match p f')

parseMatch1 :: String -> String -> String -> String -> Either PrettyError Bool
parseMatch1 contentsG contentsP contentsF name =
    case (ps, f) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right ps', Right f') ->
            case find ((name ==) . fst) ps' of
                Nothing -> Left "Padrão não encontrado no arquivo"
                Just (_, p) -> Right $ Match.Capture.match p f'
    where
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF

parseCapture :: String -> String -> String -> Either PrettyError [(String, [(Pattern, ParsedTree)])]
parseCapture contentsG contentsP contentsF =
    case (ps, f) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right ps', Right f') -> Right $ map (capture' f') ps'
    where
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF
        capture' f' (n, p) = (n, capture p f')

parseCapture1 :: String -> String -> String -> String -> Either PrettyError [(Pattern, ParsedTree)]
parseCapture1 contentsG contentsP contentsF name =
    case (ps, f) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right ps', Right f') ->
            case find ((name ==) . fst) ps' of
                Nothing -> Left "Padrão não encontrado no arquivo"
                Just (_, p) -> Right $ capture p f'
    where
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF

parseRewrite :: String -> String -> String -> String -> String -> Either PrettyError ParsedTree
parseRewrite contentsG contentsP contentsF name1 name2 =
    case (ps, f) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right ps', Right f') ->
            case (findP1 ps', findP2 ps') of
                (Nothing, _) -> Left $ "Padrão " ++ name1 ++ " não encontrado no arquivo"
                (_, Nothing) -> Left $ "Padrão " ++ name2 ++ " não encontrado no arquivo"
                (Just (_, p1), Just (_, p2)) -> Right $ rewrite p1 p2 f'
    where
        findP1 = find ((name1 ==) . fst)
        findP2 = find ((name2 ==) . fst)
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF

-------------------------------------------------------------------------------
--- IO

parseGrammarIO :: FilePath -> IO ()
parseGrammarIO f = do
    contents <- readFile f
    case parseGrammar contents of
        Left e -> print e
        Right g -> print $ pPrint g

parseValidGrammarIO :: FilePath -> IO ()
parseValidGrammarIO f = do
    contents <- readFile f
    let g = parseValidGrammar contents
    case g of
        Left e -> print e
        Right g' -> print $ pPrint g'


parsePatternsIO :: FilePath -> IO ()
parsePatternsIO f = do
    contents <- readFile f
    case parsePatterns contents of
        Left e -> print e
        Right g -> print $ pPrint g

parsePatApply :: Show a => ([NamedSynPat] -> a) -> FilePath -> IO ()
parsePatApply g f = do
    contents <- readFile f
    case parsePatterns contents of
        Left bundle -> print (errorBundlePretty bundle)
        Right xs -> print (g xs)

parseValidPatternsIO :: FilePath -> FilePath -> IO ()
parseValidPatternsIO pathGrammar pathPattern = do
    contentsG <- readFile pathGrammar
    contentsP <- readFile pathPattern
    case parseValidPatterns contentsG contentsP of
        Left e -> print e
        Right ps' -> print $ pPrint ps'


parseFileIO :: FilePath -> FilePath -> Bool -> IO ()
parseFileIO grammarFile inputFile flat = do
    contentsG <- readFile grammarFile
    contentsF <- readFile inputFile
    case parseFile contentsG contentsF of
        Left e -> print e
        Right t -> putStrLn $ if flat then flatten t else show (pPrint t)

parseMatchIO :: FilePath -> FilePath -> FilePath -> IO ()
parseMatchIO grammarFile patternFile inputFile = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseMatch contentsG contentsP contentsF of
        Left e -> print e
        Right ms -> print $ concatMap message ms
    where
        message (n, b) = n ++ if b then ": match!" else ": not match!" ++ "\n"
        
parseMatch1IO :: FilePath -> FilePath -> FilePath -> String -> IO ()
parseMatch1IO grammarFile patternFile inputFile pat = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseMatch1 contentsG contentsP contentsF pat of
        Left e -> print e
        Right b -> print $ pat ++ if b then ": match!" else ": not match!" ++ "\n"
        
parseCaptureIO :: FilePath -> FilePath -> FilePath -> IO ()
parseCaptureIO grammarFile patternFile inputFile = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseCapture contentsG contentsP contentsF of
        Left e -> print e
        Right ms -> print $ concatMap message ms
    where
        message (n, c) = "pattern " ++ n ++ ":\n" ++ concatMap printCapture c ++ "\n"
        printCapture (p, t) = show (pPrint p) ++ ":\n" ++ flatten t ++ "\n"
        
parseCapture1IO :: FilePath -> FilePath -> FilePath -> String -> IO ()
parseCapture1IO grammarFile patternFile inputFile pat = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseCapture1 contentsG contentsP contentsF pat of
        Left e -> print e
        Right m -> print $ concatMap printCapture m ++ "\n"
    where
        printCapture (p, t) = show (pPrint p) ++ ":\n" ++ flatten t ++ "\n"


parseRewriteIO :: FilePath -> FilePath -> FilePath -> String -> String -> IO ()
parseRewriteIO grammarFile patternFile inputFile pat1 pat2 = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseRewrite contentsG contentsP contentsF pat1 pat2 of
        Left e -> print e
        Right t -> putStrLn $ flatten t
