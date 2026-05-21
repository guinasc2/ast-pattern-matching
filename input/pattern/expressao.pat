-- pattern sum : E := #e1:T #e2:A
-- pattern a : E := (T := (F := "(" #m:E ")") ("*" (F := "3" ε)) ) ε
pattern a : T := (F := "(" #X:E ")") ("*" #Y:F)
pattern e : T := #X:F ("*" #Y:F)
pattern b : E := #v:E
pattern c : n := "1"
pattern d : F := "(" #X:E ")"
