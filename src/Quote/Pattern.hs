{-|
Module      : Quote.Pattern
Description : QuasiQuoter for patterns.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides QuasiQuoters for patterns, including definitions, 
expressions, and PEG operators.
-}
module Quote.Pattern 
    ( patterns
    ) where

import Quote.Base ( topLevel, parseIO, location', setPosition )
import qualified Parser.Pattern as Pattern
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Quote (dataToExpQ, QuasiQuoter(..))

patterns :: QuasiQuoter
patterns = QuasiQuoter {
        quoteExp = \ str -> do
            l <- location'
            c <- runIO $ parseIO (setPosition l *> topLevel Pattern.patterns) str
            dataToExpQ (const Nothing) c
        , quotePat  = undefined
        , quoteType = undefined
        , quoteDec  = undefined
    }