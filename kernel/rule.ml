open Basic
open Term

type pattern =
  | Var         of loc*ident*int*pattern list
  | Pattern     of loc*ident*ident*pattern list
  | Lambda      of loc*ident*pattern
  | Brackets    of term

type untyped_context = ( loc * ident ) list

type typed_context = ( loc * ident * term ) list

let get_loc_pat = function
  | Var (l,_,_,_) | Pattern (l,_,_,_)
  | Lambda (l,_,_) -> l
  | Brackets t -> get_loc t

type 'a rule = 'a * pattern * term

type typed_rule = typed_context rule
type untyped_rule = untyped_context * pattern * term

type pattern2 =
  | Joker2
  | Var2         of ident*int*int list
  | Lambda2      of ident*pattern2
  | Pattern2     of ident*ident*pattern2 array
  | BoundVar2    of ident*int*pattern2 array

type constr =
  | Linearity of int * int
  | Bracket of int * term (* change to int*term ? *)

type rule_infos = {
  l:loc;
  ctx:typed_context;
  md:ident;
  id:ident;
  args:pattern list;
  rhs:term;
  esize:int;
  l_args:pattern2 array;
  constraints:constr list;
}

type case =
  | CConst of int*ident*ident
  | CDB    of int*int
  | CLam

type abstract_pb = { position2:int (*c*) ; dbs:int LList.t (*(k_i)_{i<=n}*) ; depth2:int }
type pos = { position:int; depth:int }

type pre_context =
  | Syntactic of pos LList.t
  | MillerPattern of abstract_pb LList.t

type dtree =
  | Switch  of int * (case*dtree) list * dtree option
  | Test    of pre_context * constr list * term * dtree option

let pattern_to_term p =
  let rec aux k = function
    | Brackets t -> t
    | Pattern (l,m,v,[]) -> mk_Const l m v
    | Var (l,x,n,[]) -> mk_DB l x n
    | Pattern (l,m,v,a::args) ->
        mk_App (mk_Const l m v) (aux k a) (List.map (aux k) args)
    | Var (l,x,n,a::args) ->
        mk_App (mk_DB l x n) (aux k a) (List.map (aux k) args)
    | Lambda (l,x,pat) -> mk_Lam l x None (aux (k+1) pat)
  in
    aux 0 p

open Printf

let pp_context out ctx =
  pp_list ".\n" (fun out (_,x,ty) ->
                   Printf.fprintf out "%a: %a" pp_ident x pp_term ty )
    out (List.rev ctx)

let rec pp_pattern out = function
  | Var (_,x,n,[]) -> fprintf out "%a[%i]" pp_ident x n
  | Var (_,x,n,lst) -> fprintf out "%a[%i] %a" pp_ident x n (pp_list " " pp_pattern_wp) lst
  | Brackets t -> fprintf out "{ %a }" pp_term t
  | Pattern (_,m,v,[]) -> fprintf out "%a.%a" pp_ident m pp_ident v
  | Pattern (_,m,v,pats) -> fprintf out "%a.%a %a" pp_ident m pp_ident v (pp_list " " pp_pattern_wp) pats
  | Lambda (_,x,p) -> fprintf out "%a => %a" pp_ident x pp_pattern p
and pp_pattern_wp out = function
  | Pattern _ | Lambda _ as p -> fprintf out "(%a)" pp_pattern p
  | p -> pp_pattern out p

let pp_untyped_rule out (ctx,pat,te) =
   let pp_decl out (_,id) = pp_ident out id in
    fprintf out "[%a] %a --> %a"
      (pp_list "," pp_decl) (List.rev ctx)
      pp_pattern pat
      pp_term te

let pp_typed_rule out (ctx,pat,te) =
   let pp_decl out (_,id,ty) = fprintf out "%a:%a" pp_ident id pp_term ty in
    fprintf out "[%a] %a --> %a"
      (pp_list "," pp_decl) (List.rev ctx)
      pp_pattern pat
      pp_term te

let pp_rule_infos out r =
  pp_typed_rule out (r.ctx,Pattern(r.l,r.md,r.id,r.args),r.rhs)

let tab t = String.make (t*4) ' '

let pp_pc out = function
  | Syntactic _ -> fprintf out "Sy"
  | MillerPattern _ -> fprintf out "Mi"

let rec pp_dtree t out = function
  | Test (pc,[],te,None)   -> fprintf out "(%a) %a" pp_pc pc pp_term te
  | Test (_,[],_,def)      -> assert false
  | Test (pc,lst,te,def)  ->
      let tab = tab t in
      let aux out = function
        | Linearity (i,j) -> fprintf out "%d =l %d" i j
        | Bracket (i,j) -> fprintf out "%a =b %a" pp_term (mk_DB dloc dmark i) pp_term j
      in
      fprintf out "\n%sif %a then (%a) %a\n%selse (%a) %a" tab (pp_list " and " aux) lst
        pp_pc pc pp_term te tab pp_pc pc (pp_def (t+1)) def
  | Switch (i,cases,def)->
      let tab = tab t in
      let pp_case out = function
        | CConst (_,m,v), g ->
            fprintf out "\n%sif $%i=%a.%a then %a" tab i pp_ident m pp_ident v (pp_dtree (t+1)) g
        | CLam, g -> fprintf out "\n%sif $%i=Lambda then %a" tab i (pp_dtree (t+1)) g
        | CDB (_,n), g -> fprintf out "\n%sif $%i=DB[%i] then %a" tab i n (pp_dtree (t+1)) g
      in
        fprintf out "%a\n%sdefault: %a" (pp_list "" pp_case)
          cases tab (pp_def (t+1)) def

and pp_def t out = function
  | None        -> output_string out "FAIL"
  | Some g      -> pp_dtree t out g

let pp_rw out (m,v,i,g) =
  fprintf out "GDT for '%a.%a' with %i argument(s): %a"
    pp_ident m pp_ident v i (pp_dtree 0) g
