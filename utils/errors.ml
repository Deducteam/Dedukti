open Basics
open Term
open Rule
open Pp

let color = ref true
let errors_in_snf = ref false

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m"
  else s

let green = colored 2
let orange = colored 3
let red = colored 1

let success fmt =
  prerr_string (green "SUCCESS ") ;
  Printf.kfprintf (fun _ -> prerr_newline () ) stderr fmt

let prerr_loc lc =
  let (l,c) = of_loc lc in
    Printf.eprintf "line:%i column:%i " l c

let fail lc fmt =
  prerr_string (red "ERROR ") ;
  prerr_loc lc;
  Format.kfprintf (fun _ -> prerr_newline () ; raise Exit) Format.err_formatter fmt

let print_context2 fmt = function
  | [] -> ()
  | (_::_) as ctx ->
    Format.fprintf fmt " in context:\n%a" print_context ctx

let fail_typing_error err =
  let open Typing in
    match err with
      | KindIsNotTypable -> fail dloc "Kind is not typable."
      | ConvertibilityError (te,ctx,exp,inf) ->
          let exp = if !errors_in_snf then Env.unsafe_snf exp else exp in
          let inf = if !errors_in_snf then Env.unsafe_snf inf else inf in
            fail (get_loc te)
              "Error while typing '%a'%a.\nExpected: %a\nInferred: %a."
              print_term te print_context2 ctx print_term exp print_term inf
      | VariableNotFound (lc,x,n,ctx) ->
          fail lc "The variable '%a' was not found in context:\n"
            print_term (mk_DB lc x n) print_context ctx
      | SortExpected (te,ctx,inf) ->
          let inf = if !errors_in_snf then Env.unsafe_snf inf else inf in
            fail (Term.get_loc te)
              "Error while typing '%a'%a.\nExpected: a sort.\nInferred: %a."
              print_term te print_context2 ctx print_term inf
      | ProductExpected (te,ctx,inf) ->
          let inf = if !errors_in_snf then Env.unsafe_snf inf else inf in
            fail (get_loc te)
              "Error while typing '%a'%a.\nExpected: a product type.\nInferred: %a."
              print_term te print_context2 ctx print_term inf
      | InexpectedKind (te,ctx) ->
          fail (get_loc te)
            "Error while typing '%a'%a.\nExpected: anything but Kind.\nInferred: Kind."
            print_term te print_context2 ctx
      | DomainFreeLambda lc ->
          fail lc "Cannot infer the type of domain-free lambda."
      | CannotInferTypeOfPattern (p,ctx) ->
          fail (get_loc_pat p)
            "Error while typing '%a'%a.\nThe type could not be infered."
            print_pattern p print_context2 ctx
      | CannotSolveConstraints ((_,le,_) as r,cstr) ->
        fail (get_loc_pat le)
          "Error while typing the rewrite rule\n%a\nCannot solve typing constraints."
          print_rule r
      | BracketError1 (te,ctx) ->
        fail (get_loc te) "Error while typing the term { %a }%a.\n\
                           Brackets can only contain variables occuring \
                           on their left and cannot contain bound variables."
          print_term te print_context2 ctx
      | BracketError2 (te,ctx,ty) ->
        fail (get_loc te) "Error while typing the term { %a }%a.\n\
                           The type of brackets can only contain variables occuring\
                           on their left and cannot contains bound variables."
          print_term te print_context2 ctx
      | FreeVariableDependsOnBoundVariable (l,x,n,ctx,ty) ->
        fail l "Error while typing '%a[%i]'%a.\n\
                The type is not allowed to refer to bound variables.\n\
                Infered type:%a." print_ident x n print_context2 ctx print_term ty

let fail_dtree_error err =
  let open Dtree in
    match err with
      | BoundVariableExpected pat ->
          fail (get_loc_pat pat)
            "The pattern '%a' is not a bound variable." print_pattern pat
      | VariableBoundOutsideTheGuard te ->
          fail (get_loc te)
            "The term '%a' contains a variable bound outside the brackets."
            print_term te
      | NotEnoughArguments (lc,id,n,nb_args,exp_nb_args) ->
          fail lc "The variable '%a' is applied to %i argument(s) (expected: at least %i)."
            print_ident id nb_args exp_nb_args
      | HeadSymbolMismatch (lc,hd1,hd2) ->
          fail lc "Unexpected head symbol '%a' \ (expected '%a')."
            print_ident hd1 print_ident hd2
      | ArityMismatch (lc,id) ->
          fail lc
            "All the rewrite rules for \ the symbol '%a' should have the same arity."
            print_ident id
      | UnboundVariable (lc,x,pat) ->
          fail lc "The variables '%a' is not bound in '%a'."
            print_ident x print_pattern pat
      | AVariableIsNotAPattern (lc,id) ->
          fail lc "A variable is not a valid pattern."
      | DistinctBoundVariablesExpected (lc,x) ->
          fail lc "The variable '%a' should be applied to distinct variables."
          print_ident x
      | NonLinearRule r ->
        let (_,p,_) = r in
          fail (Rule.get_loc_pat p) "Non left-linear rewrite rule:\n%a"
            print_rule2 r

let print_cerr fmt err =
  let open Confluence in
  match  err with
  | NotConfluent cmd ->
    Format.fprintf fmt "Checker's answer: NO.\nCommand: %s" cmd
  | MaybeConfluent cmd ->
    Format.fprintf fmt "Checker's answer: MAYBE.\nCommand: %s" cmd
  | CCFailure cmd ->
    Format.fprintf fmt "Checker's answer: ERROR.\nCommand: %s" cmd

let fail_signature_error err =
  let open Signature in
    match err with
      | FailToCompileModule (lc,md) ->
          fail lc "Fail to compile dependency '%a'." print_ident md
      | UnmarshalBadVersionNumber (lc,md) ->
          fail lc "Fail to open\ module '%s' (file generated by a different version?)." md
      | UnmarshalSysError (lc,md,msg) ->
          fail lc "Fail to open module '%s' (%s)." md msg
      | UnmarshalUnknown (lc,md) ->
          fail lc "Fail to open module '%s'." md
      | SymbolNotFound (lc,md,id) ->
          fail lc "Cannot find symbol '%a.%a'." print_ident md print_ident id
      | AlreadyDefinedSymbol (lc,id) ->
          fail lc "Already declared symbol '%a'." print_ident id
      | CannotBuildDtree err -> fail_dtree_error err
      | CannotAddRewriteRules (lc,id) ->
          fail lc
            "Cannot add rewrite\ rules for the static symbol '%a'.
Add the keyword 'def' to its declaration to make the symbol '%a' definable."
            print_ident id print_ident id
      | ConfluenceErrorRules (lc,rs,cerr) ->
        fail lc "Confluence checking failed when adding the rewrite rules below.\n%a\n%a"
          print_cerr cerr (print_list "\n" print_frule) rs
      | ConfluenceErrorImport (lc,md,cerr) ->
        fail lc "Confluence checking failed when importing the module '%a'.\n%a"
          print_ident md print_cerr cerr

let fail_env_error = function
  | Env.EnvErrorSignature e -> fail_signature_error e
  | Env.EnvErrorType e -> fail_typing_error e
  | Env.KindLevelDefinition (lc,id) ->
    fail lc "Cannot add a rewrite rule for '%a' since it is a kind." print_ident id


