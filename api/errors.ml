open Basic
open Format
open Rule
open Term
open Reduction

let errors_in_snf = ref false

let snf_config = {default_cfg with strategy = Snf}

let snf t = if !errors_in_snf then Env.unsafe_reduction ~red:snf_config t else t

let color = ref true

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m" else s

let green  = colored 2
let orange = colored 3
let red    = colored 1

let success fmt =
  eprintf "%s" (green "[SUCCESS] ");
  kfprintf (fun _ -> pp_print_newline err_formatter () ) err_formatter fmt

let prerr_loc lc = eprintf "%a " pp_loc lc

(** Print an error message with given code and and exit. *)
let fail code lc fmt =
    eprintf "%s" (red "[ERROR:" ^ string_of_int code ^ "] ") ;
    prerr_loc lc;
    kfprintf (fun _ -> pp_print_newline err_formatter () ; raise Exit) err_formatter fmt

let pp_typed_context out = function
  | [] -> ()
  | _::_ as ctx -> fprintf out " in context:\n%a" pp_typed_context ctx

let fail_typing_error err =
  let open Typing in
  let code = 1 in
  match err with
  | KindIsNotTypable ->
    fail code dloc
      "Kind is not typable."
  | ConvertibilityError (te,ctx,exp,inf) ->
    fail code (get_loc te)
      "Error while typing '%a'%a.\nExpected: %a\nInferred: %a."
      pp_term te pp_typed_context ctx pp_term (snf exp) pp_term (snf inf)
  | VariableNotFound (lc,x,n,ctx) ->
    fail code lc
      "The variable '%a' was not found in context:\n"
      pp_term (mk_DB lc x n) pp_typed_context ctx
  | SortExpected (te,ctx,inf) ->
    fail code (Term.get_loc te)
      "Error while typing '%a'%a.\nExpected: a sort.\nInferred: %a."
      pp_term te pp_typed_context ctx pp_term (snf inf)
  | ProductExpected (te,ctx,inf) ->
    fail code (get_loc te)
      "Error while typing '%a'%a.\nExpected: a product type.\nInferred: %a."
      pp_term te pp_typed_context ctx pp_term (snf inf)
  | InexpectedKind (te,ctx) ->
    fail code (get_loc te)
      "Error while typing '%a'%a.\nExpected: anything but Kind.\nInferred: Kind."
      pp_term te pp_typed_context ctx
  | DomainFreeLambda lc ->
    fail code lc "Cannot infer the type of domain-free lambda."
  | CannotInferTypeOfPattern (p,ctx) ->
    fail code (get_loc_pat p)
      "Error while typing '%a'%a.\nThe type could not be infered."
      pp_pattern p pp_typed_context ctx
  | CannotSolveConstraints (r,cstr) ->
    fail code (get_loc_pat r.pat)
      "Error while typing the rewrite rule\n%a\nCannot solve typing constraints:\n%a"
      pp_untyped_rule r (pp_list "\n" (fun out (_,t1,t2) -> fprintf out "%a ~~ %a" pp_term t1 pp_term t2)) cstr
  | BracketError1 (te,ctx) ->
    fail code (get_loc te)
      "Error while typing the term { %a }%a.\n\
       Brackets can only contain variables occuring \
       on their left and cannot contain bound variables."
      pp_term te pp_typed_context ctx
  | BracketError2 (te,ctx,ty) ->
    fail code (get_loc te)
      "Error while typing the term { %a }%a.\n\
       The type of brackets can only contain variables occuring\
       on their left and cannot contains bound variables."
      pp_term te pp_typed_context ctx
  | FreeVariableDependsOnBoundVariable (l,x,n,ctx,ty) ->
    fail code l
      "Error while typing '%a[%i]'%a.\n\
       The type is not allowed to refer to bound variables.\n\
       Infered type:%a." pp_ident x n pp_typed_context ctx pp_term ty
  | Unconvertible (l,t1,t2) ->
    fail code l
      "Assertion error. Given terms are not convertible: '%a' and '%a'"
      pp_term t1 pp_term t2
  | Convertible (l,t1,t2) ->
    fail code l
      "Assertion error. Given terms are convertible: '%a' and '%a'"
      pp_term t1 pp_term t2
  | Inhabit (l,t1,t2) ->
    fail code l
      "Assertion error. '%a' is of type '%a'"
      pp_term t1 pp_term t2
  | NotImplementedFeature l ->
    fail code l
      "Feature not implemented."

let fail_dtree_error err =
  let code = 4 in
  let open Dtree in
  match err with
  | HeadSymbolMismatch (lc,cst1,cst2) ->
    fail code lc
      "Unexpected head symbol '%a' \ (expected '%a')."
      pp_name cst1 pp_name cst2
  | ArityInnerMismatch (lc, rid, id) ->
    fail code lc
      "The definable symbol '%a' inside the rewrite rules for \ '%a' should have the same arity when they are on the same column."
      pp_ident id pp_ident rid

let fail_rule_error err =
  let code = 2 in
  let open Rule in
  match err with
  | BoundVariableExpected pat ->
    fail code (get_loc_pat pat)
      "The pattern of the rule is not a Miller pattern. The pattern '%a' is not a bound variable."
      pp_pattern pat
  | VariableBoundOutsideTheGuard te ->
    fail code (get_loc te)
      "The term '%a' contains a variable bound outside the brackets."
      pp_term te
  | DistinctBoundVariablesExpected (lc,x) ->
    fail code lc
      "The pattern of the rule is not a Miller pattern. The variable '%a' should be applied to distinct variables."
      pp_ident x
  | UnboundVariable (lc,x,pat) ->
    fail code lc
      "The variables '%a' does not appear in the pattern '%a'."
      pp_ident x pp_pattern pat
  | AVariableIsNotAPattern (lc,id) ->
    fail code lc
      "A variable is not a valid pattern."
  | NotEnoughArguments (lc,id,n,nb_args,exp_nb_args) ->
    fail code lc
      "The variable '%a' is applied to %i argument(s) (expected: at least %i)."
      pp_ident id nb_args exp_nb_args
  | NonLinearRule r ->
    fail code (Rule.get_loc_pat r.pat)
      "Non left-linear rewrite rule:\n%a.\n\
       Maybe you forgot to pass the -nl option."
      pp_untyped_rule r
  | NonLinearNonEqArguments(lc,arg) ->
    fail code lc
      "For each occurence of the free variable %a, the symbol should be applied to the same number of arguments"
      pp_ident arg

let pp_cerr out err =
  let open Confluence in
  match  err with
  | NotConfluent cmd ->
    fprintf out "Checker's answer: NO.\nCommand: %s" cmd
  | MaybeConfluent cmd ->
    fprintf out "Checker's answer: MAYBE.\nCommand: %s" cmd
  | CCFailure cmd ->
    fprintf out "Checker's answer: ERROR.\nCommand: %s" cmd

let fail_signature_error err =
  let code = 3 in
  let open Signature in
  match err with
  | UnmarshalBadVersionNumber (lc,md) ->
    fail code lc "Fail to open\ module '%s' (file generated by a different version?)." md
  | UnmarshalSysError (lc,md,msg) ->
    fail code lc "Fail to open module '%s' (%s)." md msg
  | UnmarshalUnknown (lc,md) ->
    fail code lc "Fail to open module '%s'." md
  | SymbolNotFound (lc,cst) ->
    fail code lc "Cannot find symbol '%a'." pp_name cst
  | AlreadyDefinedSymbol (lc,id) ->
    fail code lc "Already declared symbol '%a'." pp_ident id
  | CannotBuildDtree err -> fail_dtree_error err
  | CannotMakeRuleInfos err -> fail_rule_error err
  | CannotAddRewriteRules (lc,id) ->
    fail code lc
      "Cannot add rewrite\ rules for the static symbol '%a'.\
       Add the keyword 'def' to its declaration to make the symbol '%a' definable."
      pp_ident id pp_ident id
  | ConfluenceErrorRules (lc,rs,cerr) ->
    fail code lc
      "Confluence checking failed when adding the rewrite rules below.\n%a\n%a"
      pp_cerr cerr (pp_list "\n" pp_rule_infos) rs
  | ConfluenceErrorImport (lc,md,cerr) ->
    fail code lc
      "Confluence checking failed when importing the module '%a'.\n%a"
      pp_mident md pp_cerr cerr
  | GuardNotSatisfied(lc, t1, t2) ->
    fail code lc
      "Error while reducing a term: a guard was not satisfied.\n\
       Expected: %a.\n\
       Found: %a"
      pp_term t1 pp_term t2
  | CouldNotExportModule file ->
    fail code dloc
      "Fail to export module '%a' to file %s."
      pp_mident (Env.get_name ()) file


let fail_env_error = function
  | Env.EnvErrorSignature e -> fail_signature_error e
  | Env.EnvErrorType      e -> fail_typing_error e
  | Env.KindLevelDefinition (lc,id) ->
    fail 5 lc "Cannot add a rewrite rule for '%a' since it is a kind." pp_ident id

let fail_if_err = function
  | OK x -> x
  | Err e -> fail_env_error e
