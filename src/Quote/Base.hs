{-|
Module      : Quote.Base
Description : Base file for QuasiQuoter.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides basic functions for QuasiQuoters.
-}
module Quote.Base 
    ( topLevel
    , parseIO
    , location'
    , setPosition
    ) where

import Parser.Base (Parser, sc)
import Text.Megaparsec
    ( parse
    , MonadParsec(eof, updateParserState)
    , SourcePos(..)
    , mkPos
    , PosState (pstateSourcePos)
    , State (statePosState), errorBundlePretty
    )
import Language.Haskell.TH
    (Q, location, Loc(loc_start, loc_filename))
import Control.Exception (throwIO)

topLevel :: Parser a -> Parser a
topLevel p = sc *> p <* eof

parseIO :: Parser a -> String -> IO a
parseIO p str =
    case parse p "" str of
        Left err -> throwIO (userError (errorBundlePretty err))
        Right a  -> return a

location' :: Q SourcePos
location' = aux <$> location
    where
        aux :: Loc -> SourcePos
        aux loc =
            let (line, col) = loc_start loc
            in SourcePos 
            {   sourceName = loc_filename loc
            ,   sourceLine = mkPos line
            ,   sourceColumn = mkPos col 
            }

setPosition :: SourcePos -> Parser ()
setPosition pos = updateParserState $ \state ->
    let pst = statePosState state
        pst' = pst { pstateSourcePos = pos }
    in state { statePosState = pst' }
