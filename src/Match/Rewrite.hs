module Match.Rewrite 
    ( replace
    , rewrite
    ) where

import Syntax.Pattern (Pattern(..))
import Syntax.ParsedTree (ParsedTree(..))
import Match.Capture (capture)
import Data.Generics (mkT, everywhere)

replace :: Pattern -> ParsedTree -> (Pattern, ParsedTree) -> ParsedTree
replace (PatVar _ name)  t (PatVar _ name', t') = if name == name' then t' else t
replace (PatNT nt p)     (ParsedNT nt' t) subst = if nt == nt'
                                                    then ParsedNT nt' (replace p t subst)
                                                    else ParsedNT nt' t
replace (PatSeq p1 p2)   (ParsedSeq t1 t2)     subst = ParsedSeq (replace p1 t1 subst) (replace p2 t2 subst)
replace (PatChoice p1 _) (ParsedChoiceLeft t)  subst = ParsedChoiceLeft $ replace p1 t subst
replace (PatChoice _ p2) (ParsedChoiceRight t) subst = ParsedChoiceRight $ replace p2 t subst
replace (PatStar p)      (ParsedStar ts)       subst = ParsedStar $ map (\ x -> replace p x subst) ts
replace _                t                     _     = t

rewrite :: Pattern -> Pattern -> ParsedTree -> ParsedTree
rewrite p p' = everywhere $ mkT (\x -> foldr ((flip . replace) p') x (capture p x))