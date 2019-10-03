open Kernel
open Basic
open Format
open Term
open Parsing

let errors_in_snf = ref false

let color = ref true

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m" else s

let green  = colored 2
(* let orange = colored 3 *)
let red    = colored 1

module type ErrorHandler =
sig
  val print_success : string option -> unit
  val graceful_fail : string option -> exn -> 'a
end

module Make (E:Env.S) : ErrorHandler =
struct
module Printer = E.Printer
open Printer

let snf t = if !errors_in_snf then E.unsafe_reduction t else t

let print_success file =
  eprintf "%s %s was successfully checked.\n"
    (green "[SUCCESS]")
    ( match file with
      | Some file -> "File '" ^ file ^ "'"
      | None      -> "Standard input" )

(* Prints an error message with file, module and line details (if provided)
   When module is provided, file is ignored. *)
let fail_exit code (errid:string) file md lc fmt =
  let eid = red ("[ERROR:" ^ errid ^ "] ") in
  begin match file, md, lc with
    | None  , None   , Some lc -> eprintf "%sAt %a: "               eid pp_loc lc
    | _     , Some md, Some lc -> eprintf "%sIn module %a, at %a: " eid pp_mident md pp_loc lc
    | Some f, None   , Some lc -> eprintf "%sIn file %s, at %a: "   eid f pp_loc lc
    | _     , Some md, None    -> eprintf "%sIn module %a: "        eid pp_mident md
    | Some f, None   , None    -> eprintf "%sIn file %s: "          eid f
    | None  , None   , None    -> eprintf "%s"                      eid
  end;
  kfprintf (fun _ -> pp_print_newline err_formatter () ; exit code) err_formatter fmt

let try_print_oneliner fmt (te,ctxt) =
  let one_liner = asprintf "%a" pp_term te in
  if String.length one_liner < 60
  then Format.fprintf fmt "'%s'%a." one_liner print_err_ctxt ctxt
  else if ctxt = [] then Format.fprintf fmt "@.%a@." print_term te
  else Format.fprintf fmt "@.%a@.----%a" print_term te print_err_ctxt ctxt

let fail_typing_error file md errid def_loc err =
  let fail lc = fail_exit 3 errid file md (Some lc) in
  let open Typing in
  match err with
  | KindIsNotTypable ->
    fail def_loc
      "Kind is not typable."
  | ConvertibilityError (te,ctx,exp,inf) ->
    fail (get_loc te)
      "Error while typing %a@.---- Expected:@.%a@.---- Inferred:@.%a@."
      try_print_oneliner (te,ctx) print_term (snf exp) print_term (snf inf)
  | AnnotConvertibilityError (lc,x,ctx,exp,inf) ->
    fail lc
      "Error while checking type annotation of variable '%a'@.---- Annotation:@.%a@.---- Inferred:@.%a@."
      try_print_oneliner (mk_DB lc x 0,ctx) print_term (snf exp) print_term (snf inf)
  | VariableNotFound (lc,x,n,ctx) ->
    fail lc
      "The variable '%a' was not found%a@."
      pp_term (mk_DB lc x n) print_err_ctxt ctx
  | SortExpected (te,ctx,inf) ->
    fail (Term.get_loc te)
      "Error while typing %a@.---- Expected: a sort.@.---- Inferred: %a."
      try_print_oneliner (te,ctx) pp_term (snf inf)
  | ProductExpected (te,ctx,inf) ->
    fail (get_loc te)
      "Error while typing %a@.---- Expected: a product type.@.---- Inferred: %a."
      try_print_oneliner (te,ctx) pp_term (snf inf)
  | InexpectedKind (te,ctx) ->
    fail (get_loc te)
      "Error while typing '%a'%a.@.---- Expected: anything but Kind.@.---- Inferred: Kind."
      pp_term te print_err_ctxt ctx
  | DomainFreeLambda lc ->
    fail lc "Cannot infer the type of domain-free lambda."
  | CannotInferTypeOfPattern (p,ctx) ->
    fail (Rule.get_loc_pat p)
      "Error while typing '%a'%a.@.The type could not be infered: \
       Probably it is not a Miller's pattern."
      Rule.pp_pattern p print_err_ctxt ctx
  | UnsatisfiableConstraints (r,(q,t1,t2)) ->
    fail (Rule.get_loc_rule r)
      "Error while typing rewrite rule.@.\
       Cannot solve typing constraints: %a ~ %a%s"
      pp_term t1 pp_term t2
      (if q > 0 then Format.sprintf " (under %i abstractions)" q else "")
  | BracketExprBoundVar (te,ctx) ->
    fail (get_loc te)
      "Error while typing the term { %a }%a.@.\
       Brackets cannot contain bound variables."
      pp_term te print_typed_context ctx
  | BracketExpectedTypeBoundVar (te,ctx,_) ->
    fail (get_loc te)
      "Error while typing the term { %a }%a.@.\
       The expected type of brackets cannot contains bound variables."
      pp_term te print_typed_context ctx
  | BracketExpectedTypeRightVar (te,ctx,_) ->
    fail (get_loc te)
      "Error while typing the term { %a }%a.@.\
       The expected type of brackets can only contain variables occuring\
       to their left."
      pp_term te print_typed_context ctx
  | TypingCircularity (l,x,n,ctx,ty) ->
    fail l
      "Typing circularity found while typing variable '%a[%i]'%a.@.\
       The expected type of variable is not allowed to refer to itself.@.\
       This is due to bracket expressions refering to this variable.@.\
       Expected type:%a." pp_ident x n print_typed_context ctx pp_term ty
  | FreeVariableDependsOnBoundVariable (l,x,n,ctx,ty) ->
    fail l
      "Error while typing '%a[%i]'%a.@.\
       The type is not allowed to refer to bound variables.@.\
       Infered type:%a." pp_ident x n print_err_ctxt ctx pp_term ty
  | Unconvertible (l,t1,t2) ->
    fail l
      "Assertion error. Given terms are not convertible: '%a' and '%a'"
      pp_term t1 pp_term t2
  | Convertible (l,t1,t2) ->
    fail l
      "Assertion error. Given terms are convertible: '%a' and '%a'"
      pp_term t1 pp_term t2
  | Inhabit (l,t1,t2) ->
    fail l
      "Assertion error. '%a' is of type '%a'"
      pp_term t1 pp_term t2
  | NotImplementedFeature l ->
    fail l
      "Feature not implemented."

let fail_dtree_error file md errid err =
  let fail lc = fail_exit 3 errid file md (Some lc) in
  let open Dtree in
  match err with
  | HeadSymbolMismatch (lc,cst1,cst2) ->
    fail lc
      "Unexpected head symbol '%a' \ (expected '%a')."
      pp_name cst1 pp_name cst2
  | ArityInnerMismatch (lc, rid, id) ->
    fail lc
      "The definable symbol '%a' inside the rewrite rules for \ '%a' should have the same arity when they are on the same column."
      pp_ident id pp_ident rid

let fail_rule_error file md errid err =
  let fail lc = fail_exit 3 errid file md (Some lc) in
  let open Rule in
  match err with
  | BoundVariableExpected pat ->
    fail (get_loc_pat pat)
      "The pattern of the rule is not a Miller pattern. The pattern '%a' is not a bound variable."
      pp_pattern pat
  | VariableBoundOutsideTheGuard te ->
    fail (get_loc te)
      "The term '%a' contains a variable bound outside the brackets."
      pp_term te
  | DistinctBoundVariablesExpected (lc,x) ->
    fail lc
      "The pattern of the rule is not a Miller pattern. The variable '%a' should be applied to distinct variables."
      pp_ident x
  | UnboundVariable (lc,x,pat) ->
    fail lc
      "The variables '%a' does not appear in the pattern '%a'."
      pp_ident x pp_pattern pat
  | AVariableIsNotAPattern (lc,_) ->
    fail lc
      "A variable is not a valid pattern."
  | NonLinearNonEqArguments(lc,arg) ->
    fail lc
      "For each occurence of the free variable %a, the symbol should be applied to the same number of arguments"
      pp_ident arg

let pp_cerr out err =
  let open Confluence in
  let cmd, ans =
    match err with
    | NotConfluent   cmd -> cmd, "NO"
    | MaybeConfluent cmd -> cmd, "MAYBE"
    | CCFailure      cmd -> cmd, "ERROR" in
  fprintf out "Checker's answer: %s.@.Command: %s" ans cmd

let fail_signature_error file md errid def_loc err =
  let fail lc = fail_exit 3 errid file md (Some lc) in
  let open Signature in
  match err with
  | UnmarshalBadVersionNumber (lc,md) -> fail lc "Fail to open\ module '%s' (file generated by a different version?)." md
  | UnmarshalSysError (lc,md,msg) ->
    fail lc "Fail to open module '%s' (%s)." md msg
  | UnmarshalUnknown (lc,md) ->
    fail lc "Fail to open module '%s'." md
  | SymbolNotFound (lc,cst) ->
    fail lc "Cannot find symbol '%a'." pp_name cst
  | AlreadyDefinedSymbol (lc,n) ->
    fail lc "Already declared symbol '%a'." pp_name n
  | CannotBuildDtree err -> fail_dtree_error file md errid err
  | CannotMakeRuleInfos err -> fail_rule_error file md errid err
  | CannotAddRewriteRules (lc,cst) ->
    fail lc
      "Cannot add rewrite\ rules for the static symbol '%a'.\
       Add the keyword 'def' to its declaration to make the symbol '%a' definable."
      pp_name cst pp_name cst
  | ConfluenceErrorRules (lc,rs,cerr) ->
    fail lc
      "Confluence checking failed when adding the rewrite rules below.@.%a@.%a"
      pp_cerr cerr (pp_list "\n" Rule.pp_rule_infos) rs
  | ConfluenceErrorImport (lc,md,cerr) ->
    fail lc
      "Confluence checking failed when importing the module '%a'.@.%a"
      pp_mident md pp_cerr cerr
  | GuardNotSatisfied(lc, t1, t2) ->
    fail lc
      "Error while reducing a term: a guard was not satisfied.@.\
       Found: %a.@.\
       Expected: %a"
      pp_term (snf t1) pp_term (snf t2)
  | CouldNotExportModule (md, file) ->
    fail def_loc
      "Fail to export module '%a' to file %s."
      pp_mident md file

let fail_dep_error fail md errid err =
  let fail lc = fail_exit 3 errid fail md (Some lc) in
  match err with
  | Dep.ModuleNotFound md ->
    fail dloc "No file for module %a in path...@." pp_mident md
  | Dep.MultipleModules (s,ss) ->
    fail dloc "Several files correspond to module %S...@. %a" s
      (pp_list "@." (fun fmt s -> Format.fprintf fmt " - %s" s)) ss
  | Dep.CircularDependencies (s,ss) ->
    fail dloc "Circular Dependency dectected for module %S...%a" s
      (pp_list "@." (fun fmt s -> Format.fprintf fmt " -> %s" s)) ss
  | Dep.NameNotFound n ->
    fail dloc "No dependencies computed for name %a...@." pp_name n
  | Dep.NoDep md ->
    fail dloc "No dependencies computed for module %a...@." pp_mident md

let code : exn -> int =
  function
  | Env.EnvError (_,_,err) ->
    begin
      match err with
      | EnvErrorType e ->
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
          | Typing.AnnotConvertibilityError _                -> 208
        end
      | EnvErrorSignature e ->
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
          | Signature.CouldNotExportModule _                 -> 402
        end
      | EnvErrorRule e ->
        begin
          match e with
          | Rule.BoundVariableExpected _                     -> 500
          | Rule.DistinctBoundVariablesExpected _            -> 501
          | Rule.VariableBoundOutsideTheGuard _              -> 502
          | Rule.UnboundVariable _                           -> 503
          | Rule.AVariableIsNotAPattern _                    -> 504
          | Rule.NonLinearNonEqArguments _                   -> 505
        end
      | EnvErrorDep e ->
        begin match e with
          | Dep.ModuleNotFound _                             -> 600
          | Dep.MultipleModules _                            -> 601
          | Dep.CircularDependencies _                       -> 602
          | Dep.NameNotFound _                               -> 603
          | Dep.NoDep _                                      -> 604
        end
      | NonLinearRule _                                      -> 506
      | NotEnoughArguments _                                 -> 507
      | KindLevelDefinition _                                -> -1
      | ParseError _                                         -> 702
      | BracketScopingError                                  -> -1
      | AssertError                                          -> 704
    end
  | Lexer.Lexer_error _                                  -> 701
  | Parser.Parse_error _                                 -> 702
  | Scoping.Scoping_error _                              -> 703
  | _                                                    -> -1

let graceful_fail file exn =
  let code = code exn in
  let errid = if code = -1 then "UNCAUGHT EXCEPTION" else string_of_int code in
  let fail md lc = fail_exit 3 errid file md (Some lc) in
  match exn with
  | Env.EnvError (md,lc,err) ->
    begin
      match err with
      | Env.EnvErrorSignature e -> fail_signature_error file md errid lc e
      | Env.EnvErrorType      e -> fail_typing_error    file md errid lc e
      | Env.EnvErrorRule      e -> fail_rule_error      file md errid    e
      | Env.EnvErrorDep       e -> fail_dep_error       file md errid    e
      | Env.NotEnoughArguments (id,_,nb_args,exp_nb_args) ->
        fail_exit 3 errid file md (Some lc)
          "The variable '%a' is applied to %i argument(s) (expected: at least %i)."
          pp_ident id nb_args exp_nb_args
      | Env.NonLinearRule rule_name ->
        fail md lc "Non left-linear rewrite rule for symbol '%a'." Rule.pp_rule_name rule_name
      | Env.KindLevelDefinition id ->
        fail md lc "Cannot add a rewrite rule for '%a' since it is a kind." pp_ident id
      | Env.ParseError s ->
        fail md lc "Parse error: %s@." s
      | Env.BracketScopingError ->
        fail md lc "Unused variables in context may create scoping ambiguity in bracket.@."
      | Env.AssertError ->
        fail md lc "Assertion failed."
    end
  | Lexer.Lexer_error(lc, msg) ->
    fail None lc "Lexer error: %s@." msg
  | Parser.Parse_error(lc, msg) ->
    fail None lc "Parsing error: %s@." msg
  | Scoping.Scoping_error(lc, msg) ->
    fail None lc "Scoping error: %s@." msg
  | Dep.Dep_error dep ->
    fail_dep_error file None errid dep
  | Sys_error err ->
    fail_exit 1 "SYSTEM" None None None "%s@." err
  | e ->
    fail_exit 3 errid file None None  "%s@." (Printexc.to_string e)

end
