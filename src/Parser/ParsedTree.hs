module Parser.ParsedTree (mkParser) where

import Syntax.Base (Terminal(..))
import Syntax.Peg (Grammar, Expression(..), expression)
import Syntax.ParsedTree (ParsedTree(..), flatten)
import Parser.Base (Parser, blank)
import Text.Megaparsec (notFollowedBy, many, choice, eof, optional, try)
import Text.Megaparsec.Char (string)
import Data.Maybe (fromJust)

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
mkParser' g (Flatten e) = ParsedT . T . flatten <$> mkParser' g e
