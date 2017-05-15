open Basic

type obj

type decl = loc * ident * Term.term

type compile_type_err

type compile_term_err

type compile_decl_err =
  | DeclarationError of decl
  | DeclarationTypeError of compile_type_err * decl
  | DeclarationTermError of compile_term_err * decl

val compile_declaration : loc -> ident -> Term.term -> (obj, compile_decl_err) error

val fail_compile_declaration : compile_decl_err -> 'a
