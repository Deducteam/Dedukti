open Basic

type obj

type compile_decl_err

type compile_defn_err

val compile_declaration : loc -> ident -> Term.term -> (obj, compile_decl_err) error

val compile_definition : loc -> ident -> Term.term -> Term.term -> (obj, compile_defn_err) error

val compile_hol_obj : obj -> unit

val fail_compile_declaration : compile_decl_err -> 'a

val fail_compile_definition : compile_defn_err -> 'a
