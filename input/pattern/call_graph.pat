pattern call : function_call := #name:identifier @space "(" @space #v:(expr_list?) ")" ε

-- pattern call2 : function_call := #name:identifier @space "(" @space @teste ")"
--pattern teste : expr_list := #v:expr1 @space (#s:sep #v2:expr1 @space)*
--pattern teste : expr_list := #v:expr1 @space (#s:sep #v1:expr1 @space) 
-- pattern teste : expr_list := #v:expr1 @space (#s:sep #v1:expr1 @space) (#s:sep #v2:expr1 @space)
-- pattern teste : expr_list := #v:expr1 @space

pattern definition : function_def := ("def" @space #name:identifier "(" @space #p:(id_list?) ")" @space ":") #block:(statement*)

pattern space : space := " "*