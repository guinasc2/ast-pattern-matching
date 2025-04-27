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
    , capture
    ) where

import Syntax.Pattern (Pattern(..))
import Syntax.ParsedTree (ParsedTree(..))
import Data.Generics (mkQ, everything)

{-|
Checks if a pattern ('Pattern') matches an AST ('ParsedTree').

The 'match' function traverses the tree and checks if the structure and values match the given pattern.

=== Usage examples:
>>> match (PatT (T "a")) (ParsedT (T "a"))
True

>>> match (PatT (T "a")) (ParsedT (T "b"))
False

@since 1.0.0
-}
match :: Pattern -> ParsedTree -> Bool
match PatEpsilon           ParsedEpsilon         = True
match (PatNot _)           ParsedNot             = True
match (PatNT nt p)         (ParsedNT nt' t)      = nt == nt' && match p t
match (PatT t)             (ParsedT t')          = t == t'
match (PatVar (Left nt) _) (ParsedNT nt' _)      = nt == nt'
match (PatVar (Right t) _) (ParsedT t')          = t == t'
match (PatSeq p1 p2)       (ParsedSeq t1 t2)     = match p1 t1 && match p2 t2
match (PatSeq p1 p2)       (ParsedIndent t1 t2)  = match p1 t1 && match p2 t2
match (PatChoice p1 _)     (ParsedChoiceLeft t)  = match p1 t
match (PatChoice _ p2)     (ParsedChoiceRight t) = match p2 t
match (PatStar p)          (ParsedStar ts)       = all (match p) ts
match (PatStarSeq ps)      (ParsedStar ts)       = length ps == length ts 
                                                   && all (uncurry match) (zip ps ts)
match _                    _                     = False

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
collect p@(PatVar (Left nt) _) tree@(ParsedNT nt' _) = if nt == nt' then [(p, tree)] else []
collect p@(PatVar (Right t) _) tree@(ParsedT t')     = if t == t' then [(p, tree)] else []
collect (PatSeq p1 p2)         (ParsedSeq t1 t2)     = collect p1 t1 ++ collect p2 t2
collect (PatSeq p1 p2)         (ParsedIndent t1 t2)  = collect p1 t1 ++ collect p2 t2
collect (PatChoice p1 _)       (ParsedChoiceLeft t)  = collect p1 t
collect (PatChoice _ p2)       (ParsedChoiceRight t) = collect p2 t
collect (PatStar p)            (ParsedStar ts)       = concatMap (collect p) ts
collect (PatStarSeq ps)        (ParsedStar ts)       = if length ps == length ts
                                                        then concat $ zipWith collect ps ts
                                                        else [] 
collect _                      _                     = []

{-|
Captures all subtrees of an AST ('ParsedTree') that match a variable ('PatVar').

The 'capture' function uses 'match' to verify matches and 'collect' to capture the subtrees.

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
capture :: Pattern -> ParsedTree -> [[(Pattern, ParsedTree)]]
capture p = everything (++) ([] `mkQ` (\x -> if match p x then [collect p x] else []))
