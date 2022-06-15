open Kernel
open Basic
open Format
open Term
open Parsers

let color = ref true

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m" else s

let green = colored 2

let orange = colored 3

let red = colored 1

let violet = colored 5

module Pp = Pp.Default

let success input =
  Format.eprintf "%s %s was successfully checked.@." (green "[SUCCESS]") input

let where file = orange (Format.asprintf "[%s]" file)

let loc_of lc = violet (Format.asprintf "[%a]" pp_loc lc)

let fail_exit ~file ~code lc fmt =
  let eid = red ("[ERROR CODE:" ^ code ^ "] ") in
  (match lc with
  | None -> eprintf "%s %s @." eid (where file)
  | Some lc -> eprintf "%s %s %s @." eid (where file) (loc_of lc));
  kfprintf
    (fun _ ->
      pp_print_newline err_formatter ();
      exit (-1))
    err_formatter fmt

let try_print_oneliner fmt (te, ctxt) =
  let one_liner = asprintf "%a" pp_term te in
  if String.length one_liner < 60 then
    Format.fprintf fmt "'%s'%a." one_liner Pp.print_err_ctxt ctxt
  else if ctxt = [] then Format.fprintf fmt "@.%a@." Pp.print_term te
  else Format.fprintf fmt "@.%a@.----%a" Pp.print_term te Pp.print_err_ctxt ctxt

let fail_sys_error ?(file = "<initialisation>") ~msg () =
  fail_exit ~file ~code:"SYSTEM" None "%s@." msg

type error_code = int

type error_msg = error_code * Basic.loc option * string

type error_handler = red:(Term.term -> Term.term) -> exn -> error_msg option

(* function which prints an exception. If not registered print default message *)
let exception_handlers : error_handler list ref = ref []

let string_of_exception ~red lc exn =
  let rec aux l =
    match l with
    | [] -> (-1, lc, Printexc.to_string exn)
    | handler :: l -> (
        match handler ~red exn with
        | None -> aux l
        | Some (code, None, exn) -> (code, lc, exn)
        | Some (code, Some lc, exn) -> (code, lc, exn))
  in
  aux !exception_handlers

let register_exception :
    (red:(Term.term -> Term.term) -> exn -> error_msg option) -> unit =
 fun f -> exception_handlers := f :: !exception_handlers

(** {2: Typing error } *)

let of_typing_error red err : error_msg =
  let open Typing in
  match err with
  | KindIsNotTypable -> (100, None, "Kind is not typable.")
  | ConvertibilityError (te, ctx, exp, inf) ->
      ( 101,
        Some (get_loc te),
        Format.asprintf
          "Error while typing %a@.---- Expected:@.%a@.---- Inferred:@.%a@."
          try_print_oneliner (te, ctx) Pp.print_term (red exp) Pp.print_term
          (red inf) )
  | VariableNotFound (lc, x, n, ctx) ->
      ( 102,
        Some lc,
        Format.asprintf "The variable '%a' was not found in context:%a@."
          pp_term (mk_DB lc x n) Pp.print_err_ctxt ctx )
  | SortExpected (te, ctx, inf) ->
      ( 103,
        Some (Term.get_loc te),
        Format.asprintf
          "Error while typing %a@.---- Expected: a sort.@.---- Inferred: %a."
          try_print_oneliner (te, ctx) pp_term (red inf) )
  | ProductExpected (te, ctx, inf) ->
      ( 104,
        Some (get_loc te),
        Format.asprintf
          "Error while typing %a@.---- Expected: a product type.@.---- \
           Inferred: %a."
          try_print_oneliner (te, ctx) pp_term (red inf) )
  | InexpectedKind (te, ctx) ->
      ( 105,
        Some (get_loc te),
        Format.asprintf
          "Error while typing '%a'%a.@.---- Expected: anything but Kind.@.---- \
           Inferred: Kind."
          pp_term te Pp.print_err_ctxt ctx )
  | DomainFreeLambda lc ->
      ( 106,
        Some lc,
        Format.asprintf "Cannot infer the type of domain-free lambda." )
  | CannotInferTypeOfPattern (p, ctx) ->
      ( 107,
        Some (Rule.get_loc_pat p),
        Format.asprintf
          "Error while typing '%a'%a.@.The type could not be infered: Probably \
           it is not a Miller's pattern."
          Rule.pp_pattern p Pp.print_err_ctxt ctx )
  | UnsatisfiableConstraints (r, (q, t1, t2)) ->
      ( 108,
        Some (Rule.get_loc_rule r),
        Format.asprintf
          "Error while typing rewrite rule.@.Cannot solve typing constraints: \
           %a ~ %a%s"
          pp_term t1 pp_term t2
          (if q > 0 then Format.sprintf " (under %i abstractions)" q else "") )
  | BracketExprBoundVar (te, ctx) ->
      ( 109,
        Some (get_loc te),
        Format.asprintf
          "Error while typing the term { %a }%a.@.Brackets cannot contain \
           bound variables."
          pp_term te Pp.print_typed_context ctx )
  | BracketExpectedTypeBoundVar (te, ctx, _) ->
      ( 200,
        Some (get_loc te),
        Format.asprintf
          "Error while typing the term { %a }%a.@.The expected type of \
           brackets cannot contains bound variables."
          pp_term te Pp.print_typed_context ctx )
  | BracketExpectedTypeRightVar (te, ctx, _) ->
      ( 201,
        Some (get_loc te),
        Format.asprintf
          "Error while typing the term { %a }%a.@.The expected type of \
           brackets can only contain variables occuringto their left."
          pp_term te Pp.print_typed_context ctx )
  | TypingCircularity (l, x, n, ctx, ty) ->
      ( 202,
        Some l,
        Format.asprintf
          "Typing circularity found while typing variable '%a[%i]'%a.@.The \
           expected type of variable is not allowed to refer to itself.@.This \
           is due to bracket expressions refering to this variable.@.Expected \
           type:%a."
          pp_ident x n Pp.print_typed_context ctx pp_term ty )
  | FreeVariableDependsOnBoundVariable (l, x, n, ctx, ty) ->
      ( 203,
        Some l,
        Format.asprintf
          "Error while typing '%a[%i]'%a.@.The type is not allowed to refer to \
           bound variables.@.Infered type:%a."
          pp_ident x n Pp.print_err_ctxt ctx pp_term ty )
  | Unconvertible (l, t1, t2) ->
      ( 204,
        Some l,
        Format.asprintf
          "Assertion error. Given terms are not convertible: '%a' and '%a'"
          pp_term t1 pp_term t2 )
  | Convertible (l, t1, t2) ->
      ( 205,
        Some l,
        Format.asprintf
          "Assertion error. Given terms are convertible: '%a' and '%a'" pp_term
          t1 pp_term t2 )
  | Inhabit (l, t1, t2) ->
      ( 206,
        Some l,
        Format.asprintf "Assertion error. '%a' is of type '%a'" pp_term t1
          pp_term t2 )
  | NotImplementedFeature l ->
      (207, Some l, Format.asprintf "Feature not implemented.")
  | AnnotConvertibilityError (lc, x, ctx, exp, inf) ->
      ( 208,
        Some lc,
        Format.asprintf
          "Error while checking type annotation of variable '%a'@.---- \
           Annotation:@.%a@.---- Inferred:@.%a@."
          try_print_oneliner
          (mk_DB lc x 0, ctx)
          Pp.print_term (red exp) Pp.print_term (red inf) )

let fail_typing_error ~red exn =
  match exn with
  | Typing.Typing_error err -> Some (of_typing_error red err)
  | _ -> None

let of_dtree_error _ err =
  let open Dtree in
  match err with
  | HeadSymbolMismatch (lc, cst1, cst2) ->
      ( 300,
        Some lc,
        Format.asprintf "Unexpected head symbol '%a'  (expected '%a')." pp_name
          cst1 pp_name cst2 )
  | ArityInnerMismatch (lc, rid, id) ->
      ( 301,
        Some lc,
        Format.asprintf
          "The definable symbol '%a' inside the rewrite rules for  '%a' should \
           have the same arity when they are on the same column."
          pp_ident id pp_ident rid )
  | ACSymbolRewritten (lc, cst, _) ->
      ( 302,
        Some lc,
        Format.asprintf
          "Rewrite rules for AC definable symbol '%a' should not have arity 0."
          pp_name cst )

let fail_dtree_error ~red exn =
  match exn with
  | Dtree.Dtree_error err -> Some (of_dtree_error red err)
  | _ -> None

let of_rule_error _ err =
  let open Rule in
  match err with
  | BoundVariableExpected (lc, pat) ->
      ( 500,
        Some lc,
        Format.asprintf
          "The pattern of the rule is not a Miller pattern. The pattern '%a' \
           is not a bound variable."
          pp_pattern pat )
  | VariableBoundOutsideTheGuard (lc, te) ->
      ( 501,
        Some lc,
        Format.asprintf
          "The term '%a' contains a variable bound outside the brackets."
          pp_term te )
  | DistinctBoundVariablesExpected (lc, x) ->
      ( 502,
        Some lc,
        Format.asprintf
          "The pattern of the rule is not a Miller pattern. The variable '%a' \
           should be applied to distinct variables."
          pp_ident x )
  | UnboundVariable (lc, x, pat) ->
      ( 503,
        Some lc,
        Format.asprintf
          "The variables '%a' does not appear in the pattern '%a'." pp_ident x
          pp_pattern pat )
  | AVariableIsNotAPattern (lc, _) ->
      (504, Some lc, Format.asprintf "A variable is not a valid pattern.")
  | NonLinearNonEqArguments (lc, arg) ->
      ( 505,
        Some lc,
        Format.asprintf
          "For each occurence of the free variable %a, the symbol should be \
           applied to the same number of arguments"
          pp_ident arg )
  | NotEnoughArguments (lc, id, _, nb_args, exp_nb_args) ->
      ( 506,
        Some lc,
        Format.asprintf
          "The variable '%a' is applied to %i argument(s) (expected: at least \
           %i)."
          pp_ident id nb_args exp_nb_args )
  | NonLinearRule (lc, rule_name) ->
      ( 507,
        Some lc,
        Format.asprintf "Non left-linear rewrite rule for symbol '%a'."
          Rule.pp_rule_name rule_name )

let fail_rule_error ~red exn =
  match exn with
  | Rule.Rule_error err -> Some (of_rule_error red err)
  | _ -> None

let pp_cerr out err =
  let open Confluence in
  let cmd, ans =
    match err with
    | NotConfluent cmd -> (cmd, "NO")
    | MaybeConfluent cmd -> (cmd, "MAYBE")
    | CCFailure cmd -> (cmd, "ERROR")
  in
  fprintf out "Checker's answer: %s.@.Command: %s" ans cmd

let of_signature_error red err =
  let open Signature in
  match err with
  | CannotBuildDtree err -> of_dtree_error red err
  | CannotMakeRuleInfos err -> of_rule_error red err
  | UnmarshalBadVersionNumber (lc, md) ->
      ( 303,
        Some lc,
        Format.asprintf
          "Fail to open module '%s' (file generated by a different version?)."
          md )
  | UnmarshalSysError (lc, md, msg) ->
      (304, Some lc, Format.asprintf "Fail to open module '%s' (%s)." md msg)
  | UnmarshalUnknown (lc, md) ->
      (305, Some lc, Format.asprintf "Fail to open module '%s'." md)
  | SymbolNotFound (lc, cst) ->
      (306, Some lc, Format.asprintf "Cannot find symbol '%a'." pp_name cst)
  | AlreadyDefinedSymbol (lc, n) ->
      (307, Some lc, Format.asprintf "Already declared symbol '%a'." pp_name n)
  | CannotAddRewriteRules (lc, cst) ->
      ( 308,
        Some lc,
        Format.asprintf
          "Cannot add rewrite rules for the static symbol '%a'.Add the keyword \
           'def' to its declaration to make the symbol '%a' definable."
          pp_name cst pp_name cst )
  | ConfluenceErrorRules (lc, rs, cerr) ->
      ( 309,
        Some lc,
        Format.asprintf
          "Confluence checking failed when adding the rewrite rules \
           below.@.%a@.%a"
          pp_cerr cerr
          (pp_list "\n" Rule.pp_rule_infos)
          rs )
  | ConfluenceErrorImport (lc, md, cerr) ->
      ( 310,
        Some lc,
        Format.asprintf
          "Confluence checking failed when importing the module '%a'.@.%a"
          pp_mident md pp_cerr cerr )
  | GuardNotSatisfied (lc, t1, t2) ->
      ( 401,
        Some lc,
        Format.asprintf
          "Error while reducing a term: a guard was not satisfied.@.Found: \
           %a.@.Expected: %a"
          pp_term (red t1) pp_term (red t2) )
  | CannotExportModule (md, exn) ->
      ( 402,
        None,
        Format.asprintf
          "Cannot export module %a. The following exception was raised:@.%s@."
          pp_mident md (Printexc.to_string exn) )
  | PrivateSymbol (lc, cst) ->
      (403, Some lc, Format.asprintf "The symbol '%a' is private." pp_name cst)
  | ExpectedACUSymbol (lc, cst) ->
      (404, Some lc, Format.asprintf "Expected ACU symbol '%a'." pp_name cst)

let fail_signature_error ~red exn =
  match exn with
  | Signature.Signature_error err -> Some (of_signature_error red err)
  | _ -> None

let fail_lexer_error ~red:_ = function
  | Lexer.Lexer_error (lc, msg) ->
      Some (701, Some lc, Format.asprintf "Lexer error: %s@." msg)
  | _ -> None

let fail_parser_error ~red:_ = function
  | Parser.Parse_error (lc, msg) ->
      Some (702, Some lc, Format.asprintf "Parsing error: %s@." msg)
  | Preterm.BetaRedexInLHS lc ->
      Some
        ( 705,
          Some lc,
          Format.asprintf
            "Parsing error: Beta-redex are not allowed in the left-hand side \
             of rewriting rule@." )
  | Preterm.AppliedGuardedTerm lc ->
      Some
        ( 706,
          Some lc,
          Format.asprintf
            "Parsing error: Applying a guarded term in the LHS of a rule is \
             not allowed@." )
  | _ -> None

let fail_scoping_error ~red:_ = function
  | Scoping.Scoping_error (lc, msg) ->
      Some (703, Some lc, Format.asprintf "Scoping error: %s@." msg)
  | _ -> None

let fail_entry_error ~red:_ = function
  | Entry.Assert_error lc ->
      Some (704, Some lc, Format.asprintf "An entry assertion has failed@.")
  | _ -> None

let _ =
  register_exception fail_typing_error;
  register_exception fail_dtree_error;
  register_exception fail_rule_error;
  register_exception fail_signature_error

let _ =
  register_exception fail_lexer_error;
  register_exception fail_parser_error;
  register_exception fail_scoping_error;
  register_exception fail_entry_error
