pattern if_def : if_stmt := (("if" @space @expr ":") #ifBlock:(statement*)) @elseBlock
pattern elseBlock : else_block := ("else" @space ":") #elseBlock:(statement*)

pattern subst : if_stmt := (("if" @space #condition:expression ":") #elseBlock:(statement*)) @elseBlock2
pattern elseBlock2 : else_block := ("else" @space ":") #ifBlock:(statement*)

pattern expr    : expression := @orExpr ε
pattern orExpr  : or_expr    := @andExpr ε
pattern andExpr : and_expr   := "not" @space #condition:comparison

pattern space : space := " "*