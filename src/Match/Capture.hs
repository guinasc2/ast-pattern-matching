{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}

module Match.Capture 
    ( match
    , capture
    ) where

import Syntax.Pattern (Pattern(..))
import Syntax.ParsedTree (ParsedTree(..))
import Data.Generics (mkQ, everything)

match :: Pattern -> ParsedTree -> Bool
match PatEpsilon           ParsedEpsilon         = True
match (PatNot _)           ParsedNot             = True
match (PatNT nt p)         (ParsedNT nt' t)      = nt == nt' && match p t
match (PatT t)             (ParsedT t')          = t == t'
match (PatVar (Left nt) _) (ParsedNT nt' _)      = nt == nt'
match (PatVar (Right t) _) (ParsedT t')          = t == t'
match (PatSeq p1 p2)       (ParsedSeq t1 t2)     = match p1 t1 && match p2 t2
match (PatChoice p1 _)     (ParsedChoiceLeft t)  = match p1 t
match (PatChoice _ p2)     (ParsedChoiceRight t) = match p2 t
match (PatStar p)          (ParsedStar ts)       = all (match p) ts
match _                    _                     = False

collect :: Pattern -> ParsedTree -> [(Pattern, ParsedTree)]
collect PatEpsilon             ParsedEpsilon         = []
collect (PatNot _)             ParsedNot             = []
collect (PatNT _ p)            (ParsedNT _ t)        = collect p t
collect (PatT _)               (ParsedT _)           = []
collect p@(PatVar (Left nt) _) tree@(ParsedNT nt' _) = if nt == nt' then [(p, tree)] else []
collect p@(PatVar (Right t) _) tree@(ParsedT t')     = if t == t' then [(p, tree)] else []
collect (PatSeq p1 p2)         (ParsedSeq t1 t2)     = collect p1 t1 ++ collect p2 t2
collect (PatChoice p1 _)       (ParsedChoiceLeft t)  = collect p1 t
collect (PatChoice _ p2)       (ParsedChoiceRight t) = collect p2 t
collect (PatStar p)            (ParsedStar ts)       = concatMap (collect p) ts
collect _                      _                     = []

capture :: Pattern -> ParsedTree -> [(Pattern, ParsedTree)]
capture p = everything (++) ([] `mkQ` (\x -> if match p x then collect p x else []))