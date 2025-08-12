pattern factorial_call : function_call := (identifier := "math.factorial") @space "(" @space #v2:(expr_list?) ")" ε
-- pattern factorial_call : function_call := (identifier := "math.factorial") @space "(" @space #v2:(expr_list?) ")" #v3:(("." primary)?)
pattern space : space := " "*

-- pattern factorial_call : function_call := (identifier := "math.factorial") "(" #v2:expr_list? ")"
-- Não vai casar se tiver dentro de uma f-string, se importar só a função ou se importar a math com outro nome
