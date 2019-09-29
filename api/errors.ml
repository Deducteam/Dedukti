open Basic
open Format
open Term
open Reduction

let errors_in_snf = ref false

let color = ref true

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m" else s

let green  = colored 2
let orange = colored 3
let red    = colored 1
let violet = colored 5

module Pp = Pp.Default

let snf env t = if !errors_in_snf then Env.unsafe_reduction env t else t

let success fmt =
  eprintf "%s" (green "[SUCCESS] ");
  kfprintf (fun _ -> pp_print_newline err_formatter () ) err_formatter fmt

let where file = orange (Format.asprintf "[%s]" file)

let loc_of lc  = violet (Format.asprintf "[%a]" pp_loc lc)

let fail_exit file code lc fmt =
  let eid = red ("[ERROR CODE:" ^ code ^ "] ") in
  begin match lc with
    | None    -> eprintf "%s %s @."        eid (where file)
    | Some lc -> eprintf "%s %s %s @."  eid (where file) (loc_of lc)
  end;
  kfprintf (fun _ -> pp_print_newline err_formatter () ; exit (-1)) err_formatter fmt

let try_print_oneliner fmt (te,ctxt) =
  let one_liner = asprintf "%a" pp_term te in
  if String.length one_liner < 60
  then Format.fprintf fmt "'%s'%a." one_liner Pp.print_err_ctxt ctxt
  else if ctxt = [] then Format.fprintf fmt "@.%a@." Pp.print_term te
  else Format.fprintf fmt "@.%a@.----%a" Pp.print_term te Pp.print_err_ctxt ctxt


type error_msg = Basic.loc option * string

type error_handler = red:(Term.term -> Term.term) -> exn -> error_msg option

let of_typing_error ~red err : error_msg =
  let open Typing in
  match err with
  | KindIsNotTypable ->
    None,  "Kind is not typable."
  | ConvertibilityError (te,ctx,exp,inf) ->
    Some (get_loc te),
    Format.asprintf "Error while typing %a@.---- Expected:@.%a@.---- Inferred:@.%a@."
      try_print_oneliner (te,ctx) Pp.print_term (red exp) Pp.print_term (red inf)
  | VariableNotFound (lc,x,n,ctx) ->
    Some lc, Format.asprintf
      "The variable '%a' was not found in context:%a@."
      pp_term (mk_DB lc x n) Pp.print_err_ctxt ctx
  | SortExpected (te,ctx,inf) ->
    Some (Term.get_loc te), Format.asprintf
      "Error while typing %a@.---- Expected: a sort.@.---- Inferred: %a."
      try_print_oneliner (te,ctx) pp_term (red inf)
  | ProductExpected (te,ctx,inf) ->
    Some (get_loc te), Format.asprintf
      "Error while typing %a@.---- Expected: a product type.@.---- Inferred: %a."
      try_print_oneliner (te,ctx) pp_term (red inf)
  | InexpectedKind (te,ctx) ->
    Some (get_loc te), Format.asprintf
      "Error while typing '%a'%a.@.---- Expected: anything but Kind.@.---- Inferred: Kind."
      pp_term te Pp.print_err_ctxt ctx
  | DomainFreeLambda lc ->
    Some lc, Format.asprintf "Cannot infer the type of domain-free lambda."
  | CannotInferTypeOfPattern (p,ctx) ->
    Some (Rule.get_loc_pat p), Format.asprintf
      "Error while typing '%a'%a.@.The type could not be infered: \
       Probably it is not a Miller's pattern."
      Rule.pp_pattern p Pp.print_err_ctxt ctx
  | UnsatisfiableConstraints (r,(q,t1,t2)) ->
    Some (Rule.get_loc_rule r), Format.asprintf
      "Error while typing rewrite rule.@.\
       Cannot solve typing constraints: %a ~ %a%s"
      pp_term t1 pp_term t2
      (if q > 0 then Format.sprintf " (under %i abstractions)" q else "")
  | BracketExprBoundVar (te,ctx) ->
    Some (get_loc te), Format.asprintf
      "Error while typing the term { %a }%a.@.\
       Brackets cannot contain bound variables."
      pp_term te Pp.print_typed_context ctx
  | BracketExpectedTypeBoundVar (te,ctx,ty) ->
    Some (get_loc te), Format.asprintf
      "Error while typing the term { %a }%a.@.\
       The expected type of brackets cannot contains bound variables."
      pp_term te Pp.print_typed_context ctx
  | BracketExpectedTypeRightVar (te,ctx,ty) ->
    Some (get_loc te), Format.asprintf
      "Error while typing the term { %a }%a.@.\
       The expected type of brackets can only contain variables occuring\
       to their left."
      pp_term te Pp.print_typed_context ctx
  | TypingCircularity (l,x,n,ctx,ty) ->
    Some l, Format.asprintf
      "Typing circularity found while typing variable '%a[%i]'%a.@.\
       The expected type of variable is not allowed to refer to itself.@.\
       This is due to bracket expressions refering to this variable.@.\
       Expected type:%a." pp_ident x n Pp.print_typed_context ctx pp_term ty
  | FreeVariableDependsOnBoundVariable (l,x,n,ctx,ty) ->
    Some l, Format.asprintf
      "Error while typing '%a[%i]'%a.@.\
       The type is not allowed to refer to bound variables.@.\
       Infered type:%a." pp_ident x n Pp.print_err_ctxt ctx pp_term ty
  | Unconvertible (l,t1,t2) ->
    Some l, Format.asprintf
      "Assertion error. Given terms are not convertible: '%a' and '%a'"
      pp_term t1 pp_term t2
  | Convertible (l,t1,t2) ->
    Some l, Format.asprintf
      "Assertion error. Given terms are convertible: '%a' and '%a'"
      pp_term t1 pp_term t2
  | Inhabit (l,t1,t2) ->
    Some l, Format.asprintf
      "Assertion error. '%a' is of type '%a'"
      pp_term t1 pp_term t2
  | NotImplementedFeature l ->
    Some l, Format.asprintf "Feature not implemented."

let fail_typing_error ~red exn =
  match exn with
  | Typing.Typing_error err -> Some (of_typing_error red err)
  | _ -> None

let of_dtree_error red err =
  let open Dtree in
  match err with
  | HeadSymbolMismatch (lc,cst1,cst2) ->
    Some lc, Format.asprintf
      "Unexpected head symbol '%a' \ (expected '%a')."
      pp_name cst1 pp_name cst2
  | ArityInnerMismatch (lc, rid, id) ->
    Some lc, Format.asprintf
      "The definable symbol '%a' inside the rewrite rules for \ '%a' should have the same arity when they are on the same column."
      pp_ident id pp_ident rid

let fail_dtree_error ~red exn =
  match exn with
  | Dtree.Dtree_error err -> Some (of_dtree_error red err)
  | _ -> None

let of_rule_error red err =
  let open Rule in
  match err with
  | BoundVariableExpected(lc, pat) ->
    Some lc, Format.asprintf
      "The pattern of the rule is not a Miller pattern. The pattern '%a' is not a bound variable."
      pp_pattern pat
  | VariableBoundOutsideTheGuard(lc, te) ->
    Some lc, Format.asprintf
      "The term '%a' contains a variable bound outside the brackets."
      pp_term te
  | DistinctBoundVariablesExpected (lc,x) ->
    Some lc, Format.asprintf
      "The pattern of the rule is not a Miller pattern. The variable '%a' should be applied to distinct variables."
      pp_ident x
  | UnboundVariable (lc,x,pat) ->
    Some lc, Format.asprintf
      "The variables '%a' does not appear in the pattern '%a'."
      pp_ident x pp_pattern pat
  | AVariableIsNotAPattern (lc,id) ->
    Some lc, Format.asprintf
      "A variable is not a valid pattern."
  | NonLinearNonEqArguments(lc,arg) ->
    Some lc, Format.asprintf
      "For each occurence of the free variable %a, the symbol should be applied to the same number of arguments"
      pp_ident arg
  | NotEnoughArguments (lc,id,n,nb_args,exp_nb_args) ->
    Some lc, Format.asprintf
      "The variable '%a' is applied to %i argument(s) (expected: at least %i)."
      pp_ident id nb_args exp_nb_args
  |NonLinearRule (lc,rule_name) ->
    Some lc, Format.asprintf
      "Non left-linear rewrite rule for symbol '%a'." Rule.pp_rule_name rule_name

let fail_rule_error ~red exn =
  match exn with
  | Rule.Rule_error err -> Some (of_rule_error red err)
  | _ -> None

let pp_cerr out err =
  let open Confluence in
  let cmd, ans =
    match err with
    | NotConfluent   cmd -> cmd, "NO"
    | MaybeConfluent cmd -> cmd, "MAYBE"
    | CCFailure      cmd -> cmd, "ERROR" in
  fprintf out "Checker's answer: %s.@.Command: %s" ans cmd

let of_signature_error red err =
  let open Signature in
  match err with
  | UnmarshalBadVersionNumber (lc,md) ->
    Some lc, Format.asprintf "Fail to open\ module '%s' (file generated by a different version?)." md
  | UnmarshalSysError (lc,md,msg) ->
    Some lc, Format.asprintf "Fail to open module '%s' (%s)." md msg
  | UnmarshalUnknown (lc,md) ->
    Some lc, Format.asprintf "Fail to open module '%s'." md
  | SymbolNotFound (lc,cst) ->
    Some lc, Format.asprintf "Cannot find symbol '%a'." pp_name cst
  | AlreadyDefinedSymbol (lc,n) ->
    Some lc, Format.asprintf "Already declared symbol '%a'." pp_name n
  | CannotBuildDtree err -> of_dtree_error red err
  | CannotMakeRuleInfos err -> of_rule_error red err
  | CannotAddRewriteRules (lc,cst) ->
    Some lc, Format.asprintf
      "Cannot add rewrite\ rules for the static symbol '%a'.\
       Add the keyword 'def' to its declaration to make the symbol '%a' definable."
      pp_name cst pp_name cst
  | ConfluenceErrorRules (lc,rs,cerr) ->
    Some lc, Format.asprintf
      "Confluence checking failed when adding the rewrite rules below.@.%a@.%a"
      pp_cerr cerr (pp_list "\n" Rule.pp_rule_infos) rs
  | ConfluenceErrorImport (lc,md,cerr) ->
    Some lc, Format.asprintf
      "Confluence checking failed when importing the module '%a'.@.%a"
      pp_mident md pp_cerr cerr
  | GuardNotSatisfied(lc, t1, t2) ->
    Some lc, Format.asprintf
      "Error while reducing a term: a guard was not satisfied.@.\
       Found: %a.@.\
       Expected: %a"
      pp_term (red t1) pp_term (red t2)
  | CannotExportModule(md,exn) ->
    None, Format.asprintf
      "Cannot export module %a. The following exception was raised:@.\
       %s@." pp_mident md (Printexc.to_string exn)

let fail_signature_error ~red exn =
  match exn with
  | Signature.Signature_error err -> Some (of_signature_error red err)
  | _ -> None


type error_code = int

let exception_code_handlers : (exn -> error_code option) list ref = ref []

let rec register_exception_code : (exn -> error_code option) -> unit = fun f ->
  exception_code_handlers := f :: !exception_code_handlers

let use_code_handlers =
  fun exn ->
    let rec aux l =
      match l with
      | [] -> -1
      | handler::l ->
        match handler exn with
        | None -> aux l
        | Some i -> i
    in
    aux !exception_code_handlers

let code : exn -> int =
  function
  | Typing.Typing_error e ->
    begin
      match e with
      | Typing.KindIsNotTypable                          -> 100
      | Typing.ConvertibilityError _                     -> 101
      | Typing.VariableNotFound _                        -> 102
      | Typing.SortExpected _                            -> 103
      | Typing.ProductExpected _                         -> 104
      | Typing.InexpectedKind _                          -> 105
      | Typing.DomainFreeLambda _                        -> 106
      | Typing.CannotInferTypeOfPattern _                -> 107
      | Typing.UnsatisfiableConstraints _                -> 108
      | Typing.BracketExprBoundVar _                     -> 109
      | Typing.BracketExpectedTypeBoundVar _             -> 200
      | Typing.BracketExpectedTypeRightVar _             -> 201
      | Typing.TypingCircularity _                       -> 202
      | Typing.FreeVariableDependsOnBoundVariable _      -> 203
      | Typing.Unconvertible _                           -> 204
      | Typing.Convertible _                             -> 205
      | Typing.Inhabit _                                 -> 206
      | Typing.NotImplementedFeature _                   -> 207
    end
  | Signature.Signature_error e ->
    begin
      match e with
      | Signature.CannotBuildDtree e ->
        begin match e with
          | Dtree.HeadSymbolMismatch _                   -> 300
          | Dtree.ArityInnerMismatch _                   -> 301
        end
      | Signature.CannotMakeRuleInfos _                  -> 302
      | Signature.UnmarshalBadVersionNumber _            -> 303
      | Signature.UnmarshalSysError _                    -> 304
      | Signature.UnmarshalUnknown _                     -> 305
      | Signature.SymbolNotFound _                       -> 306
      | Signature.AlreadyDefinedSymbol _                 -> 307
      | Signature.CannotAddRewriteRules _                -> 308
      | Signature.ConfluenceErrorRules _                 -> 309
      | Signature.ConfluenceErrorImport _                -> 400
      | Signature.GuardNotSatisfied _                    -> 401
      | Signature.CannotExportModule _                   -> 402
    end
  | Rule.Rule_error e ->
    begin
      match e with
      | Rule.BoundVariableExpected _                     -> 500
      | Rule.DistinctBoundVariablesExpected _            -> 501
      | Rule.VariableBoundOutsideTheGuard _              -> 502
      | Rule.UnboundVariable _                           -> 503
      | Rule.AVariableIsNotAPattern _                    -> 504
      | Rule.NonLinearNonEqArguments _                   -> 505
      | Rule.NonLinearRule _                             -> 506
      | Rule.NotEnoughArguments _                        -> 507
    end
  | Lexer.Lexer_error _                                  -> 701
  | Parser.Parse_error _                                 -> 702
  | Scoping.Scoping_error _                              -> 703
  | Entry.Assert_error _                                 -> 704
  | exn                                                  -> use_code_handlers exn

let string_of_code exn =
  let code = code exn in
  if code = -1 then "UNKNOWN" else string_of_int code

(* function which prints an exception. If not registered print default message *)
let exception_handlers : error_handler list ref = ref []

let use_exception_handlers =
  fun env lc exn ->
  let snf t = snf env t in
  let rec aux l =
    match l with
    | [] -> lc, Printexc.to_string exn
    | handler::l ->
      match handler ~red:snf exn with
      | None -> aux l
      | Some(None,exn)     -> lc,exn
      | Some(Some lc,exn) -> lc,exn
  in
  aux !exception_handlers

let register_exception : (red:(Term.term -> Term.term) -> exn -> error_msg option) -> unit =
  fun f -> exception_handlers := f :: !exception_handlers

let _ =
  register_exception fail_typing_error;
  register_exception fail_dtree_error;
  register_exception fail_rule_error;
  register_exception fail_signature_error

let fail_env_error env lc exn =
  let code = string_of_code exn in
  let file = Env.get_filename env in
  let fail lc = fail_exit file code (Some lc) in
  match exn with
  (* | Lexer.Lexer_error(lc, msg) ->
   *   Some lc, Format.asprintf "Lexer error: %s@." msg
   * | Parser.Parse_error(lc, msg) ->
   *   Some lc, Format.asprintf "Parsing error: %s@." msg
   * | Scoping.Scoping_error(lc, msg) ->
   *   Some lc, Format.asprintf "Scoping error: %s@." msg
   * | Entry.Assert_error lc ->
   *   Some lc, Format.asprintf "An entry assertion has failed@." *)
  | exn -> fail_env_error env lc exn

let fail_sys_error file msg = fail_exit file "SYSTEM" None "%s@." msg
