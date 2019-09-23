open Basic
open Format
open Term
open Reduction
open Pp

let errors_in_snf = ref false

let color = ref true

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m" else s

let green  = colored 2
let orange = colored 3
let red    = colored 1

module type ErrorHandler =
sig
  type t
  val success : ('a, Format.formatter, unit) format -> 'a
  val fail_exit : int -> string -> mident option -> loc option -> ('a, Format.formatter, unit) format -> 'a
  val fail_env_error : t -> (mident option * loc * Env.env_error) -> 'a
  val fail_sys_error : string -> 'a
end

module Make (E:Env.S) : (ErrorHandler with type t = E.t) =
struct
module Printer = E.Printer
open Printer

type t = E.t

let snf sg t = if !errors_in_snf then E.unsafe_reduction sg t else t

let success fmt =
  eprintf "%s" (green "[SUCCESS] ");
  kfprintf (fun _ -> pp_print_newline err_formatter () ) err_formatter fmt

let fail_exit code errid md lc fmt =
  let eid = red ("[ERROR:" ^ errid ^ "] ") in
  begin match md, lc with
    | Some md, None    -> eprintf "%sIn module %a: "        eid pp_mident md
    | None   , Some lc -> eprintf "%sAt %a: "               eid pp_loc lc
    | Some md, Some lc -> eprintf "%sIn module %a, at %a: " eid pp_mident md pp_loc lc
    | None   , None    -> eprintf "%s"                      eid
  end;
  kfprintf (fun _ -> pp_print_newline err_formatter () ; exit code) err_formatter fmt

let try_print_oneliner sg fmt (te,ctxt) =
  let one_liner = asprintf "%a" pp_term te in
  if String.length one_liner < 60
  then Format.fprintf fmt "'%s'%a." one_liner (print_err_ctxt sg) ctxt
  else if ctxt = [] then Format.fprintf fmt "@.%a@." (print_term sg) te
  else Format.fprintf fmt "@.%a@.----%a" (print_term sg) te (print_err_ctxt sg) ctxt

let fail_typing_error sg md errid def_loc err =
  let fail lc = fail_exit 3 errid md (Some lc) in
  let open Typing in
  match err with
  | KindIsNotTypable ->
    fail def_loc
      "Kind is not typable."
  | ConvertibilityError (te,ctx,exp,inf) ->
    fail (get_loc te)
      "Error while typing %a@.---- Expected:@.%a@.---- Inferre<d:@.%a@."
      (try_print_oneliner sg) (te,ctx) (print_term sg) (snf sg exp) (print_term sg) (snf sg inf)
  | VariableNotFound (lc,x,n,ctx) ->
    fail lc
      "The variable '%a' was not found in context:%a@."
      pp_term (mk_DB lc x n) (print_err_ctxt sg) ctx
  | SortExpected (te,ctx,inf) ->
    fail (Term.get_loc te)
      "Error while typing %a@.---- Expected: a sort.@.---- Inferred: %a."
      (try_print_oneliner sg) (te,ctx) pp_term (snf sg inf)
  | ProductExpected (te,ctx,inf) ->
    fail (get_loc te)
      "Error while typing %a@.---- Expected: a product type.@.---- Inferred: %a."
      (try_print_oneliner sg) (te,ctx) pp_term (snf sg inf)
  | InexpectedKind (te,ctx) ->
    fail (get_loc te)
      "Error while typing '%a'%a.@.---- Expected: anything but Kind.@.---- Inferred: Kind."
      pp_term te (print_err_ctxt sg)ctx
  | DomainFreeLambda lc ->
    fail lc "Cannot infer the type of domain-free lambda."
  | CannotInferTypeOfPattern (p,ctx) ->
    fail (Rule.get_loc_pat p)
      "Error while typing '%a'%a.@.The type could not be infered: \
       Probably it is not a Miller's pattern."
      Rule.pp_pattern p (print_err_ctxt sg) ctx
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
      pp_term te (print_typed_context sg) ctx
  | BracketExpectedTypeBoundVar (te,ctx,ty) ->
    fail (get_loc te)
      "Error while typing the term { %a }%a.@.\
       The expected type of brackets cannot contains bound variables."
      pp_term te (print_typed_context sg) ctx
  | BracketExpectedTypeRightVar (te,ctx,ty) ->
    fail (get_loc te)
      "Error while typing the term { %a }%a.@.\
       The expected type of brackets can only contain variables occuring\
       to their left."
      pp_term te (print_typed_context sg) ctx
  | TypingCircularity (l,x,n,ctx,ty) ->
    fail l
      "Typing circularity found while typing variable '%a[%i]'%a.@.\
       The expected type of variable is not allowed to refer to itself.@.\
       This is due to bracket expressions refering to this variable.@.\
       Expected type:%a." pp_ident x n (print_typed_context sg) ctx pp_term ty
  | FreeVariableDependsOnBoundVariable (l,x,n,ctx,ty) ->
    fail l
      "Error while typing '%a[%i]'%a.@.\
       The type is not allowed to refer to bound variables.@.\
       Infered type:%a." pp_ident x n (print_err_ctxt sg) ctx pp_term ty
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

let fail_dtree_error md errid err =
  let fail lc = fail_exit 3 errid md (Some lc) in
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

let fail_rule_error md errid err =
  let fail lc = fail_exit 3 errid md (Some lc) in
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
  | AVariableIsNotAPattern (lc,id) ->
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

let fail_signature_error sg md errid def_loc err =
  let fail lc = fail_exit 3 errid md (Some lc) in
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
  | CannotBuildDtree err -> fail_dtree_error md errid err
  | CannotMakeRuleInfos err -> fail_rule_error md errid err
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
      pp_term (snf sg t1) pp_term (snf sg t2)
  | CouldNotExportModule (md, file) ->
    fail def_loc
      "Fail to export module '%a' to file %s."
      pp_mident md file

let fail_dep_error md errid err =
  let fail lc = fail_exit 3 errid md (Some lc) in
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

let code err =
  let open Env in
  match err with
  | ParseError _      -> 1
  | BracketScopingError -> 42
  | EnvErrorType e -> begin match e with
      | Typing.KindIsNotTypable -> 2
      | Typing.ConvertibilityError _ -> 3
      | Typing.VariableNotFound _ -> 4
      | Typing.SortExpected _ -> 5
      | Typing.ProductExpected _ -> 6
      | Typing.InexpectedKind _ -> 7
      | Typing.DomainFreeLambda _ -> 8
      | Typing.CannotInferTypeOfPattern _ -> 9
      | Typing.UnsatisfiableConstraints _ -> 10
      | Typing.BracketExprBoundVar _ -> 11
      | Typing.BracketExpectedTypeBoundVar _ -> 12
      | Typing.BracketExpectedTypeRightVar _ -> 12
      | Typing.TypingCircularity _ -> 12
      (* TODO offset everything to have a fresh code here. *)
      | Typing.FreeVariableDependsOnBoundVariable _ -> 13
      | Typing.Unconvertible _ -> 14
      | Typing.Convertible _ -> 15
      | Typing.Inhabit _ -> 16
      | Typing.NotImplementedFeature _ -> 17
    end
  | EnvErrorSignature e -> begin match e with
      | Signature.CannotBuildDtree e -> begin match e with
          | Dtree.HeadSymbolMismatch _ -> 18
          | Dtree.ArityInnerMismatch _ -> 19
        end
      | Signature.CannotMakeRuleInfos e -> begin match e with
          | Rule.BoundVariableExpected _ -> 20
          | Rule.VariableBoundOutsideTheGuard _ -> 21
          | Rule.DistinctBoundVariablesExpected _ -> 22
          | Rule.UnboundVariable _ -> 23
          | Rule.AVariableIsNotAPattern _ -> 24
          | Rule.NonLinearNonEqArguments _ -> 27
        end
      | Signature.UnmarshalBadVersionNumber _ -> 28
      | Signature.UnmarshalSysError _ -> 29
      | Signature.UnmarshalUnknown _ -> 30
      | Signature.SymbolNotFound _ -> 31
      | Signature.AlreadyDefinedSymbol _ -> 32
      | Signature.CannotAddRewriteRules _ -> 33
      | Signature.ConfluenceErrorRules _ -> 34
      | Signature.ConfluenceErrorImport _ -> 35
      | Signature.GuardNotSatisfied _ -> 36
      | Signature.CouldNotExportModule _ -> 37
    end
  | EnvErrorRule e -> begin match e with
      | Rule.BoundVariableExpected _ -> 40
      | Rule.DistinctBoundVariablesExpected (_,_) -> 41
      | Rule.VariableBoundOutsideTheGuard _ -> 42
      | Rule.UnboundVariable (_,_,_) -> 43
      | Rule.AVariableIsNotAPattern (_,_) -> 44
      | Rule.NonLinearNonEqArguments (_,_) -> 45
    end
  | EnvErrorDep e -> begin match e with
      | Dep.ModuleNotFound _ -> 46
      | Dep.MultipleModules _ -> 47
      | Dep.CircularDependencies _ -> 48
      | Dep.NameNotFound _ -> 49
      | Dep.NoDep _ -> 50
    end
  | NotEnoughArguments _  -> 25
  | NonLinearRule _       -> 26
  | KindLevelDefinition _ -> 38
  | AssertError           -> 39

let fail_env_error sg (md,lc,err) =
  let errid = string_of_int (code err) in
  let fail lc = fail_exit 3 errid md (Some lc) in
  match err with
  | Env.EnvErrorSignature e -> fail_signature_error sg md errid lc e
  | Env.EnvErrorType      e -> fail_typing_error    sg md errid lc e
  | Env.EnvErrorRule      e -> fail_rule_error      md errid    e
  | Env.EnvErrorDep       e -> fail_dep_error       md errid    e
  | Env.NotEnoughArguments (id,n,nb_args,exp_nb_args) ->
    fail_exit 3 errid md (Some lc)
      "The variable '%a' is applied to %i argument(s) (expected: at least %i)."
      pp_ident id nb_args exp_nb_args
  | Env.NonLinearRule rule_name ->
    fail lc "Non left-linear rewrite rule for symbol '%a'." Rule.pp_rule_name rule_name
  | Env.KindLevelDefinition id ->
    fail lc "Cannot add a rewrite rule for '%a' since it is a kind." pp_ident id
  | Env.ParseError s ->
    fail lc "Parse error: %s@." s
  | Env.BracketScopingError ->
    fail lc "Unused variables in context may create scoping ambiguity in bracket.@."
  | Env.AssertError ->
    fail lc "Assertion failed."

let fail_sys_error msg = fail_exit 1 "SYSTEM" None None "%s@." msg

end
