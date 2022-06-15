open Basic
open Term
open Rule
open Format
open Ac

type dtree_error =
  | HeadSymbolMismatch of loc * name * name
  | ArityInnerMismatch of loc * ident * ident
  | ACSymbolRewritten of loc * name * int

exception Dtree_error of dtree_error

type miller_var = {
  arity : int;  (** Arity of the meta variable *)
  depth : int;
      (** Depth under which this occurence of the meta variable is considered *)
  vars : int list;  (** The list of local DB indices of argument variables*)
  mapping : int array;
      (** The mapping from all local DB indices for either -1 or position
        in the list of argument variables
    *)
}

let fo_var : miller_var = {arity = 0; depth = 0; vars = []; mapping = [||]}

let mapping_of_vars (depth : int) (arity : int) (vars : int list) : int array =
  let arr = Array.make depth (-1) in
  List.iteri (fun i n -> arr.(n) <- arity - i - 1) vars;
  arr

type var_p = int * miller_var

(* TODO: add loc to this to better handle errors *)
type 'a eq_problem = miller_var * 'a

type 'a ac_problem = int * ac_ident * int * var_p list * 'a

type pre_matching_problem = {
  pm_eq_problems : int eq_problem list LList.t;
  pm_ac_problems : int ac_problem list;
  pm_arity : int array;
}

let pp_var_type fmt (i, {arity; vars; _}) =
  if arity = 0 then fprintf fmt "%i" i
  else fprintf fmt "%i[%a]" i (pp_list " " pp_print_int) vars

let pp_eq_problem vp pp_a fmt (args, t) =
  fprintf fmt "%a = %a" pp_var_type (vp, args) pp_a t

let pp_eq_problems sep pp_a fmt (vp, prbs) =
  fprintf fmt "%a" (pp_list sep (pp_eq_problem vp pp_a)) prbs

let pp_njoks fmt n = if n > 0 then fprintf fmt " + %i _" n

let pp_ac_problem pp_rhs fmt (_, aci, joks, vars, terms) =
  fprintf fmt "{ %a%a } =(%a) { %a }"
    (pp_list " , " pp_var_type)
    vars pp_njoks joks pp_ac_ident aci pp_rhs terms

let pp_pos fmt p = fprintf fmt "stack.%a" pp_print_int p

let pp_pre_matching_problem sep fmt mp =
  fprintf fmt "[ %a | %a ]"
    (pp_llist sep (pp_eq_problems sep pp_pos))
    (LList.mapi (fun i c -> (i, c)) mp.pm_eq_problems)
    (pp_list sep (pp_ac_problem pp_pos))
    mp.pm_ac_problems

type case = CConst of int * name * bool | CDB of int * int | CLam

type atomic_problem = {a_pos : int; a_depth : int; a_args : int array}

type matching_problem = atomic_problem LList.t

type dtree =
  | Switch of int * (case * dtree) list * dtree option
  | Test of
      Rule.rule_name * pre_matching_problem * constr list * term * dtree option
  | Fetch of int * case * dtree * dtree option
  | ACEmpty of int * dtree * dtree option

(** Type of decision forests *)
type t = algebra * (int * dtree) list

let empty = (Free, [])

(** Return first pair (ar,tree) in given list such that ar <= stack_size *)
let find_dtree stack_size (alg, l) =
  let rec aux = function
    | [] -> None
    | hd :: tl -> if fst hd <= stack_size then Some hd else aux tl
  in
  (alg, aux l)

let mk_AC_set cst pat1 pat2 =
  let rec flatten acc = function
    | [] -> acc
    | LPattern (cst', args) :: tl
      when name_eq cst cst' && Array.length args == 2 ->
        flatten acc (args.(0) :: args.(1) :: tl)
    | t :: tl -> flatten (t :: acc) tl
  in
  LACSet (cst, flatten [] [pat1; pat2])

(******************************************************************************)

(*
 * there is one matrix per head symbol that represents all the rules associated to that symbol.
 * col_depth:   [ (n_0)          (n_1)          ...       (n_k)   ]
 * first:       [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 * others:      [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 *                  ...           ...           ...       ...
 *              [ pats.(0)      pats.(1)        ...     pats.(k)  ]
 *
 *              n_i records the depth of the column (number of binders under which it stands)
 *)
type matrix = {
  col_depth : int array;
  first : rule_infos;
  others : rule_infos list;
}

(** Merge and flatten the first two argument of AC headed patterns
    into the LACSet representation:
      + (+ r s) (t u) ...  --> ...
    becomes
      +{r s t u} ...  --> ...
  *)
let merge_AC_arguments =
  let aux r =
    let f = function
      | 0 -> mk_AC_set r.cst r.pats.(0) r.pats.(1)
      | i -> r.pats.(i - 1)
    in
    let npats = Array.init (Array.length r.pats - 1) f in
    {r with pats = npats}
  in
  List.map aux

(** Append extra rule when necessary :
      +{1 0} --> r    becomes  +{1 0 X} --> +{r X}
      +{X X} --> r    becomes  +{X X Y} --> +{r Y}  (TODO)
      +{X 0} --> r    is left unchanged (X already a "scraps collecting" variable)
  *)
let expand_AC_rules =
  let rec aux acc = function
    | [] -> List.rev acc
    | r :: tl -> (
        assert (Array.length r.pats == 1);
        let is_linear_var = function
          | LJoker -> true
          | LVar (_, i, []) -> not (List.mem i r.nonlinear)
          | _ -> false
        in
        match r.pats.(0) with
        | LACSet (cst, args) ->
            let new_acc =
              if List.exists is_linear_var args then r :: acc
              else
                (* +{pats} --> r    where pats contains no variable. *)
                let newr =
                  (* becomes  +{pats,x} --> + r x  with x fresh variable *)
                  {
                    r with
                    esize = r.esize + 1;
                    rhs =
                      mk_App (mk_Const dloc cst) r.rhs
                        [mk_DB dloc dmark r.esize];
                    pats = [|LACSet (cst, LVar (dmark, r.esize, []) :: args)|];
                  }
                in
                newr :: r :: acc
            in
            aux new_acc tl
        | _ -> assert false)
  in
  aux []

(* mk_matrix lst builds a matrix out of the non-empty list of rules [lst]
   *  It is checked that all rules have the same head symbol and arity.
*)
let mk_matrix (ac : bool) (arity : int) (ri : rule_infos list) : matrix =
  let rules = List.filter (fun x -> List.length x.args <= arity) ri in
  assert (rules <> []);
  (* At least one rule should correspond to the given arity. *)
  let f r =
    let ar = Array.length r.pats in
    assert (ar <= arity);
    (* This guaranted in the of_rules function.  *)
    if ar == arity then r
    else
      (* Edit rule r with too low arity : add extra arguments*)
      let tail =
        Array.init (arity - ar) (fun i -> LVar (dmark, i + r.esize, []))
      in
      let new_args =
        List.map
          (function LVar (x, n, []) -> mk_DB dloc x n | _ -> assert false)
          (Array.to_list tail)
      in
      {
        r with
        esize = r.esize + arity - ar;
        rhs = mk_App2 r.rhs new_args;
        pats = Array.append r.pats tail;
      }
  in
  let rules = List.map f rules in
  let rules = if ac && arity > 1 then merge_AC_arguments rules else rules in
  let rules = if ac && arity == 2 then expand_AC_rules rules else rules in
  {
    first = List.hd rules;
    others = List.tl rules;
    col_depth = Array.make arity 0;
  }

(* Remove a line of the matrix [mx] and return None if the new matrix is Empty. *)
let pop mx =
  match mx.others with
  | [] -> None
  | f :: o -> Some {mx with first = f; others = o}

let filter (f : rule_infos -> bool) (mx : matrix) : matrix option =
  match List.filter f (mx.first :: mx.others) with
  | [] -> None
  | f :: o -> Some {mx with first = f; others = o}

let get_rule_filter f c r = f r.pats.(c)

(* Keeps only the rules with a lambda on column [c] *)
let filter_on_lambda = function
  | LLambda _ | LJoker | LVar _ -> true
  | LACSet (_, s) -> List.exists (function LLambda _ -> true | _ -> false) s
  | _ -> false

(* Keeps only the rules with a bound variable of index [n] on column [c] *)
let filter_on_bound_variable nargs n = function
  | LVar _ | LJoker -> true
  | LBoundVar (_, n', args) -> n' == n && Array.length args == nargs
  | LACSet _ -> assert false
  | _ -> false

(* Keeps only the rules with a pattern head by [cst]
   applied to [nargs] arguments. *)
let filter_on_pattern nargs cst = function
  | LVar _ | LJoker -> true
  | LPattern (cst', ar') -> name_eq cst cst' && Array.length ar' == nargs
  | LACSet _ -> assert false
  | _ -> false

(* Keeps only the rules with a joker or a variable on column [c] *)
let filter_default (mx : matrix) (c : int) : matrix option =
  filter
    (fun r ->
      match r.pats.(c) with
      | LVar _ | LJoker -> true
      | LLambda _ | LPattern _ | LBoundVar _ -> false
      | LACSet _ -> assert false)
    mx

let partition_AC_rules c f rules =
  let rec aux (keep, def) = function
    | [] -> (keep, def)
    | r :: tl -> (
        match r.pats.(c) with
        | LVar _ | LJoker -> aux (r :: keep, r :: def) tl
        | LACSet (_, pats) ->
            if f pats then aux (r :: keep, def) tl else aux (keep, r :: def) tl
        | _ -> aux (keep, r :: def) tl)
  in
  aux ([], []) rules

let filter_AC_on_empty_set = function LACSet (_, []) -> true | _ -> false

let filter_AC_on_lambda s =
  List.exists (function LLambda _ -> true | _ -> false) s

let filter_AC_on_bound_variable nargs n s =
  List.exists
    (function
      | LBoundVar (_, n', args) -> n' == n && Array.length args == nargs
      | _ -> false)
    s

let filter_AC_on_pattern nargs cst s =
  List.exists
    (function
      | LPattern (cst', ar') -> name_eq cst cst' && Array.length ar' == nargs
      | _ -> false)
    s

let case_eq a b =
  match (a, b) with
  | CLam, CLam -> true
  | CDB (ar, n), CDB (ar', n') -> ar == ar' && n == n'
  | CConst (ar, cst, _), CConst (ar', cst', _) -> ar == ar' && name_eq cst cst'
  | _, _ -> false

let case_of_pattern (is_AC : name -> bool) : wf_pattern -> case option =
  function
  | LVar _ | LJoker -> None
  | LPattern (cst, pats) ->
      Some
        (CConst (Array.length pats, cst, is_AC cst && Array.length pats >= 2))
  | LBoundVar (_, n, pats) -> Some (CDB (Array.length pats, n))
  | LLambda _ -> Some CLam
  | LACSet _ -> assert false

let case_pattern_match (case : case) (pat : wf_pattern) : bool =
  match (case, pat) with
  | CConst (lpats, c', _), LPattern (c, pats) ->
      name_eq c c' && lpats == Array.length pats
  | CDB (lpats, n'), LBoundVar (_, n, pats) ->
      n' == n && lpats == Array.length pats
  | CLam, LLambda _ -> true
  | _ -> false

let specialize_empty_AC_rule (c : int) (r : rule_infos) : rule_infos =
  {
    r with
    pats =
      Array.init (Array.length r.pats) (fun i ->
          if i == c then LJoker else r.pats.(i));
  }

let specialize_AC_rule case (c : int) (nargs : int) (r : rule_infos) :
    rule_infos =
  let size = Array.length r.pats in
  let new_pats_c, pat =
    match r.pats.(c) with
    | LACSet (cst, l) ->
        let rec remove_case acc = function
          | [] -> assert false
          | hd :: tl ->
              if case_pattern_match case hd then
                (LACSet (cst, List.rev_append acc tl), hd)
              else remove_case (hd :: acc) tl
        in
        remove_case [] l
    | LVar _ | LJoker -> (r.pats.(c), LJoker)
    | _ -> assert false
  in
  let aux i =
    if i < size then if i == c then new_pats_c else r.pats.(i)
    else
      (* size <= i < size+nargs *)
      match (pat, case) with
      | LPattern (cst, pats2), CConst (_, cst', true) ->
          assert (name_eq cst cst');
          assert (nargs >= 1);
          assert (Array.length pats2 == nargs + 1);
          if i == size then mk_AC_set cst pats2.(0) pats2.(1)
          else pats2.(i - size + 1)
      | LPattern (_, pats2), _ | LBoundVar (_, _, pats2), _ ->
          assert (Array.length pats2 == nargs);
          pats2.(i - size)
      | LLambda (_, p), _ ->
          assert (nargs == 1);
          p
      | LJoker, _ -> LJoker
      | _ -> assert false
  in
  {r with pats = Array.init (size + nargs) aux}

(* Specialize the rule [r] on column [c]
 * i.e. replace colum [c] with a joker and append [nargs] new column at the end.
 * These new columns contain
 * - the arguments if column [c] is a pattern
 * - or the body if column [c] is a lambda
 * - or Jokers otherwise
* *)
let specialize_rule case (c : int) (nargs : int) (r : rule_infos) : rule_infos =
  let size = Array.length r.pats in
  let aux i =
    if i < size then
      if i == c then match r.pats.(c) with LVar _ as v -> v | _ -> LJoker
      else r.pats.(i)
    else
      (* size <= i < size+nargs *)
      let check_args id pats =
        if Array.length pats != nargs then
          raise (Dtree_error (ArityInnerMismatch (r.l, Basic.id r.cst, id)));
        pats.(i - size)
      in
      match r.pats.(c) with
      | LJoker | LVar _ -> LJoker
      | LBoundVar (id, _, pats2) -> check_args id pats2
      | LLambda (_, p) ->
          assert (nargs == 1);
          p
      | LACSet _ -> assert false
      | LPattern (cst, pats2) -> (
          match case with
          | CConst (_, cst', true) ->
              (* AC const *)
              assert (name_eq cst cst');
              assert (Array.length pats2 == nargs + 1 && nargs != 0);
              if i == size then mk_AC_set cst pats2.(0) pats2.(1)
              else pats2.(i - size + 1)
          | _ -> check_args (id cst) pats2)
  in
  {r with pats = Array.init (size + nargs) aux}

(* Specialize the col_infos field of a matrix.
 * Invalid for specialization by lambda. *)
let spec_col_depth (c : int) (nargs : int) (col_depth : int array) : int array =
  let size = Array.length col_depth in
  let aux i =
    if i < size then col_depth.(i) else (* < size+nargs *) col_depth.(c)
  in
  Array.init (size + nargs) aux

(* Specialize the col_infos field of a matrix: the lambda case. *)
let spec_col_depth_l (c : int) (col_depth : int array) : int array =
  let size = Array.length col_depth in
  let aux i =
    if i < size then col_depth.(i) else (*i == size *) col_depth.(c) + 1
  in
  Array.init (size + 1) aux

(* Specialize the matrix [mx] on AC-empty column [c] *)
let specialize_ACEmpty (mx : matrix) (c : int) : matrix * matrix option =
  let rules_suc, rules_def =
    List.partition
      (get_rule_filter filter_AC_on_empty_set c)
      (mx.first :: mx.others)
  in
  match rules_suc with
  | [] -> assert false
  | first :: others -> (
      ( {
          mx with
          first = specialize_empty_AC_rule c first;
          others = List.map (specialize_empty_AC_rule c) others;
        },
        match rules_def with
        | [] -> None
        | f :: o -> Some {mx with first = f; others = o} ))

(* Specialize the matrix [mx] on column [c] *)
let specialize_AC (mx : matrix) (c : int) (case : case) : matrix * matrix option
    =
  let nargs, part_f =
    match case with
    | CLam -> (1, filter_AC_on_lambda)
    | CDB (nargs, n) -> (nargs, filter_AC_on_bound_variable nargs n)
    | CConst (nargs, cst, _) -> (nargs, filter_AC_on_pattern nargs cst)
  in
  let rules_suc, rules_def =
    partition_AC_rules c part_f (mx.first :: mx.others)
  in
  let nargs = nargs - match case with CConst (_, _, true) -> 1 | _ -> 0 in
  let new_cn =
    match case with
    | CLam -> spec_col_depth_l c mx.col_depth
    | _ -> spec_col_depth c nargs mx.col_depth
  in
  match rules_suc with
  | [] -> assert false
  | first :: others -> (
      ( {
          first = specialize_AC_rule case c nargs first;
          others = List.map (specialize_AC_rule case c nargs) others;
          col_depth = new_cn;
        },
        match rules_def with
        | [] -> None
        | f :: o -> Some {mx with first = f; others = o} ))

(* Specialize the matrix [mx] on column [c] *)
let specialize (mx : matrix) (c : int) (case : case) : matrix =
  let nargs, filter_f =
    match case with
    | CLam -> (1, filter_on_lambda)
    | CDB (nargs, n) -> (nargs, filter_on_bound_variable nargs n)
    | CConst (nargs, cst, _) -> (nargs, filter_on_pattern nargs cst)
  in
  let mx_opt = filter (get_rule_filter filter_f c) mx in
  let add_args = nargs - match case with CConst (_, _, true) -> 1 | _ -> 0 in
  let new_cn =
    match case with
    | CLam -> spec_col_depth_l c mx.col_depth
    | _ -> spec_col_depth c add_args mx.col_depth
  in
  match mx_opt with
  | None -> assert false
  | Some mx2 ->
      {
        first = specialize_rule case c add_args mx2.first;
        others = List.map (specialize_rule case c add_args) mx2.others;
        col_depth = new_cn;
      }

(******************************************************************************)

let rec partition_AC (is_AC : name -> bool) : wf_pattern list -> case = function
  | [] -> assert false
  | hd :: tl -> (
      match case_of_pattern is_AC hd with
      | Some c -> c
      | None -> partition_AC is_AC tl)

let partition (is_AC : name -> bool) (mx : matrix) (c : int) : case list =
  let aux lst li =
    match case_of_pattern is_AC li.pats.(c) with
    | Some c -> if List.exists (case_eq c) lst then lst else c :: lst
    | None -> lst
  in
  List.fold_left aux [] (mx.first :: mx.others)

(******************************************************************************)

let get_first_term mx = mx.first.rhs

let get_first_constraints mx = mx.first.constraints

(* Extracts the matching_problem from the first line. *)
let get_first_matching_problem (get_algebra : name -> algebra) mx =
  let esize = mx.first.esize in
  let arity = Array.make esize (-1) in
  let eq_pbs = Array.make esize [] in
  let ac_pbs = ref [] in
  Array.iteri
    (fun i p ->
      let depth = mx.col_depth.(i) in
      match p with
      | LJoker -> ()
      | LVar (_, n, args) ->
          assert (depth <= n && n < esize + depth);
          let n = n - depth in
          let len = List.length args in
          if arity.(n) == -1 then arity.(n) <- len
          else assert (arity.(n) == len);
          let miller =
            {
              depth;
              arity = len;
              vars = args;
              mapping = mapping_of_vars depth len args;
            }
          in
          eq_pbs.(n) <- (miller, i) :: eq_pbs.(n)
      | LACSet (cst, patl) ->
          let fetch_metavars (joks, vars) = function
            | LJoker -> (joks + 1, vars)
            | LVar (_, n, args) ->
                assert (depth <= n && n < esize + depth);
                let n = n - depth in
                let len = List.length args in
                if arity.(n) == -1 then arity.(n) <- len
                else assert (arity.(n) == len);
                let miller =
                  {
                    depth;
                    arity = len;
                    vars = args;
                    mapping = mapping_of_vars depth len args;
                  }
                in
                let nvars = (n, miller) :: vars in
                (joks, nvars)
            | _ -> assert false
          in
          let njoks, metavars = List.fold_left fetch_metavars (0, []) patl in
          ac_pbs :=
            (depth, (cst, get_algebra cst), njoks, metavars, i) :: !ac_pbs
      | _ -> assert false)
    mx.first.pats;
  assert (Array.for_all (fun x -> x >= 0) arity);
  {
    pm_eq_problems = LList.of_array eq_pbs;
    pm_ac_problems = !ac_pbs;
    pm_arity = arity;
  }

(******************************************************************************)

(*  TODO: check at some point that no neutral element can occur in a pattern *)
let rec non_var_pat = function
  | LVar _ | LJoker -> false
  | LACSet (_, []) -> true
  | LACSet (_, patl) -> List.exists non_var_pat patl
  | _ -> true

(* Give the index of the first non variable column *)
let choose_column mx =
  let rec aux i =
    if i < Array.length mx.first.pats then
      if non_var_pat mx.first.pats.(i) then Some i else aux (i + 1)
    else None
  in
  aux 0

(* Construct a decision tree out of a matrix *)
let rec to_dtree get_algebra (mx : matrix) : dtree =
  let is_AC cst = is_AC (get_algebra cst) in
  match choose_column mx with
  (* There are only variables on the first line of the matrix *)
  | None ->
      Test
        ( mx.first.name,
          get_first_matching_problem get_algebra mx,
          get_first_constraints mx,
          get_first_term mx,
          map_opt (to_dtree get_algebra) (pop mx) )
  (* Pattern on the first line at column c *)
  | Some c -> (
      match mx.first.pats.(c) with
      | LACSet (_, []) ->
          let mx_suc, mx_def = specialize_ACEmpty mx c in
          ACEmpty
            ( c,
              to_dtree get_algebra mx_suc,
              map_opt (to_dtree get_algebra) mx_def )
      | LACSet (_, l) ->
          let case = partition_AC is_AC l in
          let mx_suc, mx_def = specialize_AC mx c case in
          Fetch
            ( c,
              case,
              to_dtree get_algebra mx_suc,
              map_opt (to_dtree get_algebra) mx_def )
      | _ ->
          (* Carry parameter (false) above  *)
          let cases = partition is_AC mx c in
          let aux ca = (ca, to_dtree get_algebra (specialize mx c ca)) in
          Switch
            ( c,
              List.map aux cases,
              map_opt (to_dtree get_algebra) (filter_default mx c) ))

(******************************************************************************)

(** Adds a new arity to a (reverse) sorted list of distincts arities *)
let rec add l ar =
  match l with
  | [] -> [ar]
  | hd :: tl ->
      if ar > hd then ar :: l
      else if ar == hd then l (* ar is already in l *)
      else hd :: add tl ar

let of_rules name get_algebra rs : t =
  let alg = get_algebra name in
  let ac = is_AC alg in
  let arities = ref [] in
  List.iter
    (fun x ->
      if not (name_eq x.cst name) then
        raise (Dtree_error (HeadSymbolMismatch (x.l, x.cst, name)));
      let arity = List.length x.args in
      if ac && arity == 0 (* + --> ... is forbidden when + is AC  *) then
        raise (Dtree_error (ACSymbolRewritten (x.l, x.cst, arity)));
      (* The rule    + l   --> r
         requires    + l x --> + r x
         to behave as expected, ie matching (1+l) "below the AC head") *)
      if ac && arity == 1 then arities := add !arities 2;
      (* Also add a rule of arity 2. *)
      arities := add !arities arity)
    rs;
  let sorted_arities = List.fold_left add [] !arities in
  (* reverse sorted list of all rewrite rules arities. *)
  let aux ar =
    let m = mk_matrix ac ar rs in
    (ar, to_dtree get_algebra m)
  in
  (alg, List.map aux sorted_arities)

(******************************************************************************)

let pp_AC_args fmt i =
  if i < 2 then fprintf fmt "%i args" i
  else if i == 2 then fprintf fmt "AC args"
  else fprintf fmt "AC args, %i args" (i - 2)

let rec pp_dtree t fmt dtree =
  (* FIXME: Use format boxes here instead of manual tabs. *)
  let tab = String.init (1 + (t * 2)) (fun i -> if i == 0 then '\n' else ' ') in
  match dtree with
  | Test (_, mp, [], te, _)
    when mp.pm_ac_problems = []
         && List.for_all (fun c -> c = []) (LList.lst mp.pm_eq_problems) ->
      fprintf fmt "%s%a" tab pp_term te
  | Test (name, mp, [], te, def) ->
      fprintf fmt "%stry %a :%s    %a%sthen %a%selse %a" tab pp_rule_name name
        tab
        (pp_pre_matching_problem (tab ^ "      "))
        mp tab pp_term te tab
        (pp_def (t + 1))
        def
  | Test (name, mp, cstr, te, def) ->
      fprintf fmt "%stry %a :%s    %a%sunder constraints %a%sthen %a%selse %a"
        tab pp_rule_name name tab
        (pp_pre_matching_problem (tab ^ "      "))
        mp tab (pp_list ", " pp_constr) cstr tab pp_term te tab
        (pp_def (t + 1))
        def
  | Switch (i, cases, def) ->
      let pp_case out = function
        | CConst (nargs, name, false), g ->
            fprintf out "%sif $%i = %a (%i args) then %a" tab i pp_name name
              nargs
              (pp_dtree (t + 1))
              g
        | CConst (nargs, name, true), g ->
            fprintf out "%sif $%i = %a (%a) then %a" tab i pp_name name
              pp_AC_args nargs
              (pp_dtree (t + 1))
              g
        | CDB (nargs, n), g ->
            fprintf out "%sif $%i = DB[%i] (%i args) then %a" tab i n nargs
              (pp_dtree (t + 1))
              g
        | CLam, g ->
            fprintf out "%sif $%i = Lambda then %a" tab i (pp_dtree (t + 1)) g
      in
      fprintf fmt "%a%sdefault: %a" (pp_list "" pp_case) cases tab
        (pp_def (t + 1))
        def
  | ACEmpty (i, tree_suc, tree_def) ->
      fprintf fmt "%sif $%i (AC flattened) is empty then %a%selse %a" tab i
        (pp_dtree (t + 1))
        tree_suc tab
        (pp_def (t + 1))
        tree_def
  | Fetch (i, case, tree_suc, tree_def) ->
      (match case with
      | CConst (nargs, name, false) ->
          fprintf fmt "%sif $%i is AC applied to %a (%i args) then %a%selse %a"
            tab i pp_name name nargs
      | CConst (nargs, name, true) ->
          fprintf fmt "%sif $%i is AC applied to %a (%a) then %a%selse %a" tab i
            pp_name name pp_AC_args nargs
      | CDB (nargs, n) ->
          fprintf fmt
            "%sif $%i is AC applied to DB[%i] (%i args) then %a%selse %a" tab i
            n nargs
      | CLam ->
          fprintf fmt "%sif $%i is AC applied to Lambda then %a%selse %a" tab i)
        (pp_dtree (t + 1))
        tree_suc tab
        (pp_def (t + 1))
        tree_def

and pp_def t fmt = function
  | None -> fprintf fmt "FAIL"
  | Some g -> pp_dtree t fmt g

let pp_dtree fmt dtree = pp_dtree 0 fmt dtree

let pp_rw fmt (i, g) =
  fprintf fmt "When applied to %i argument(s): %a" i pp_dtree g

let pp_dforest fmt = function
  | Free, [] -> fprintf fmt "No GDT.@."
  | AC, [] -> fprintf fmt "No GDT for AC symbol.@."
  | ACU _, [] -> fprintf fmt "No GDT for ACU symbol.@."
  | _, trees -> fprintf fmt "%a@." (pp_list "\n" pp_rw) trees
