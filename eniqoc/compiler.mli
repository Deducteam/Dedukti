val compile_declaration : Basic.name -> Term.term  -> Ast.declaration

val compile_definition : Basic.name -> Term.term -> Term.term -> Ast.definition

val add_declaration : Ast.declaration -> unit

val add_definition : Ast.definition -> unit

val init_ast : Basic.ident -> unit

val get_ast : unit -> Ast.ast
