{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}

{-|
Module      : Match.Capture
Description : Functions for matching and capturing patterns in syntax trees.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions to check pattern matching ('Pattern') in
AST ('ParsedTree') and capture corresponding subtrees.
-}
module Match.Capture
    ( match
    , match'
    , collect
    , capture
    ) where

import Syntax.Pattern (Pattern(..))
import Syntax.ParsedTree (ParsedTree(..), ParsedTreeZipper, goDown, goLeft, goRight, goUp, pullFromRight, ofExpression)
import Data.Generics (mkQ, everything)
import Syntax.Peg (Grammar)
import Data.Maybe (isJust)

{-|
Checks if a pattern ('Pattern') matches an AST ('ParsedTree').

The 'match'' function traverses the tree and checks if the structure and values match the given pattern.

=== Usage examples:
>>> match' (PatT (T "a")) (ParsedT (T "a"))
True

>>> match' (PatT (T "a")) (ParsedT (T "b"))
False

@since 1.0.0
-}
match' :: Grammar -> Pattern -> ParsedTreeZipper -> Maybe [(Pattern, ParsedTree)]
match' g p@(PatVar e n)         z@(t, _) =
    if ofExpression g e t
        then Just [(p, t)]
        else match' g (PatVar e n) =<< e2
    where
        up = goUp z
        e1 = (\(expr, z') -> (,z') <$> pullFromRight expr) =<< up
        e2 = goLeft =<< e1
match' _ PatEpsilon           (ParsedEpsilon, _)         = 
    Just []
match' _ (PatNot _)           (ParsedNot, _)             = 
    Just []
match' g (PatNT nt p)         z@(ParsedNT nt' _, _)      =
    if nt == nt'
        then match' g p =<< goDown z
        else Nothing
match' _ (PatT t)             (ParsedT t', _)          = 
    if t == t' then Just [] else Nothing
match' g (PatSeq p1 p2)       z@(ParsedSeq _ _, _)   = 
    (++) <$> (match' g p1 =<< goLeft z) <*> (match' g p2 =<< goRight z)
match' g (PatSeq p1 p2)       z@(ParsedIndent _ _, _) = 
    (++) <$> (match' g p1 =<< goLeft z) <*> (match' g p2 =<< goRight z) 
match' g (PatChoice p1 _)     z@(ParsedChoiceLeft _, _)  = 
    match' g p1 =<< goDown z
match' g (PatChoice _ p2)     z@(ParsedChoiceRight _, _) = 
    match' g p2 =<< goDown z
match' g (PatStar p)          (ParsedStar ts, _)         = 
    foldr (\ x xs -> (++) <$> match' g p (x, []) <*> xs) (Just []) ts
match' g (PatStarSeq ps)      (ParsedStar ts, _)         = 
    if length ps == length ts
    then foldr (\ x y -> (++) <$> x <*> y) (Just []) $ zipWith (\ x y -> match' g x (y, [])) ps ts
    else Nothing
match' _ _                    _                          = 
    Nothing

{-|
Checks if a pattern ('Pattern') matches any subtree of an AST ('ParsedTree').

=== Usage examples:

=== Usage examples:
>>> match (PatT (T "a")) (ParsedT (T "a"))
True

>>> match (PatT (T "a")) (ParsedSeq (ParsedT (T "a")) (ParsedT (T "b")))
True

@since 1.0.0
-}
match :: Grammar -> Pattern -> ParsedTree -> Bool
match g p = everything (||) (False `mkQ` (isJust . match' g p . (, [])))

{-|
Collects the variables of a pattern and the subtrees they matched.

The 'collect' function is used internally to capture pairs of patterns and corresponding subtrees.

@since 1.0.0
-}
collect :: Pattern -> ParsedTree -> [(Pattern, ParsedTree)]
collect PatEpsilon             ParsedEpsilon         = []
collect (PatNot _)             ParsedNot             = []
collect (PatNT _ p)            (ParsedNT _ t)        = collect p t
collect (PatT _)               (ParsedT _)           = []
-- collect p@(PatVar (Left nt) _) tree@(ParsedNT nt' _) = if nt == nt' then [(p, tree)] else []
-- collect p@(PatVar (Right t) _) tree@(ParsedT t')     = if t == t' then [(p, tree)] else []
collect (PatSeq p1 p2)         (ParsedSeq t1 t2)     = collect p1 t1 ++ collect p2 t2
collect (PatSeq p1 p2)         (ParsedIndent t1 t2)  = collect p1 t1 ++ collect p2 (ParsedStar t2)
collect (PatChoice p1 _)       (ParsedChoiceLeft t)  = collect p1 t
collect (PatChoice _ p2)       (ParsedChoiceRight t) = collect p2 t
collect (PatStar p)            (ParsedStar ts)       = concatMap (collect p) ts
collect (PatStarSeq ps)        (ParsedStar ts)       = if length ps == length ts
                                                        then concat $ zipWith collect ps ts
                                                        else []
collect _                      _                     = []

{-|
Captures all subtrees of an AST ('ParsedTree') that match a variable ('PatVar').

The 'capture' function uses 'match'' to verify matches and 'collect' to capture the subtrees.

=== Usage examples:

>>> let pattern = PatSeq (PatT (T "a")) (PatVar (Right (T "b")) "B")
>>> let tree = ParsedSeq (ParsedT (T "a")) (ParsedT (T "b"))
>>> capture pattern tree
[[(PatVar (Right (T "b")) "B",ParsedT (T "b"))]]

>>> let pattern = (PatVar (Right (T "a")) "A")
>>> let tree = ParsedSeq (ParsedT (T "a")) (ParsedT (T "b"))
>>> capture pattern tree
[[(PatVar (Right (T "a")) "A",ParsedT (T "a"))]]

@since 1.0.0
-}
capture :: Grammar -> Pattern -> ParsedTree -> [[(Pattern, ParsedTree)]]
capture g p = everything (++) ([] `mkQ` (\ x -> case match' g p (x, []) of 
                                                Just y -> [y] 
                                                Nothing -> []))
