(** {2 Identifiers (hashconsed strings)} *)

type ident = string
let string_of_ident s = s
let ident_eq s1 s2 = s1==s2 || s1=s2
let pp_ident = output_string

let ident_cmp s1 s2 =
  if s1==s2 then 0 else String.compare s1 s2

module WS = Weak.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

let shash       = WS.create 251
let hstring     = WS.merge shash
let empty       = hstring ""

module IdentMap = Map.Make(struct type t = ident let compare = ident_cmp end)

(** {2 Variables} *)

module Var = struct
  type t = (ident * int)

  let _count = ref 0
  let _next() =
    let n = !_count in
    incr _count;
    n

  let fresh_of_ident name = (name,_next())
  let fresh (name,_) = fresh_of_ident name
  let equal (n1,i1)(n2,i2) = ident_eq n1 n2 && i1 = i2
  let compare (n1,i1)(n2,i2) =
    if i1=i2 then String.compare n1 n2 else Pervasives.compare i1 i2
  let hash v = Hashtbl.hash v

  let ident (i,_) = i
  let pp out (name,i) = Printf.fprintf out "%s[%d]" name i
end

module VarMap = Map.Make(Var)
module VarSet = Set.Make(Var)

module VarTbl = struct
  include Hashtbl.Make(Var)

  let get_default tbl var ~x0 =
    try find tbl var
    with Not_found -> x0
end

type 'a subst = 'a VarMap.t

let subst_empty = VarMap.empty

let subst_is_empty = VarMap.is_empty

let subst_bind subst v x = VarMap.add v x subst

let subst_find subst v =
  try Some (VarMap.find v subst)
  with Not_found -> None

(** {2 Lists with Length} *)

module LList = struct
  type 'a t= {
    len : int;
    lst : 'a list;
  }

  let cons x {len;lst} = {len=len+1; lst=x::lst}
  let nil = {len=0;lst=[];}
  let len x = x.len
  let lst x = x.lst
  let is_empty x = x.len = 0

  let make ~len lst =
    assert (List.length lst = len);
    {lst;len}

  let make_unsafe ~len lst = {len;lst}

  let map f {len;lst} = {len; lst=List.map f lst}
  let append_l {len;lst} l = {len=len+List.length l; lst=lst@l}

  let nth l i = List.nth l.lst i

  let remove i {len;lst} =
    let rec aux c lst = match lst with
      | []        -> assert false
      | x::lst'   -> if c==0 then lst' else x::(aux (c-1) lst')
    in
    {len=len-1; lst=aux i lst}
end


(** {2 Localization} *)

type loc = int*int
let dloc = (0,0)
let mk_loc l c = (l,c)
let of_loc l = l

(** {2 Parsing} *)

type token =
  | UNDERSCORE  of loc
  | TYPE        of loc
  | RIGHTSQU
  | RIGHTPAR
  | RIGHTBRA
  | QID         of ( loc * ident * ident )
  | NAME        of ( loc * ident )
  | LONGARROW
  | LEFTSQU
  | LEFTPAR
  | LEFTBRA
  | ID          of ( loc * ident )
  | FATARROW
  | EOF
  | DOT
  | DEF
  | COMMA
  | COLON
  | ARROW
  | WHNF        of loc
  | HNF         of loc
  | SNF         of loc
  | STEP        of loc
  | INFER       of loc
  | CONV        of loc
  | CHECK       of loc
  | PRINT       of loc
  | GDT         of loc
  | OTHER       of ( loc * string )
  | STRING      of string

exception EndOfFile

(** {2 PreTerms/PrePatterns} *)

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm * preterm
  | PrePi   of loc * ident option * preterm * preterm

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list
  | PJoker       of loc

type pdecl      = loc * ident * preterm
type pcontext   = pdecl list
type prule      = loc * pdecl list * ident * prepattern list * preterm

(** {2 Terms/Patterns} *)

type term =
  | Kind                                (* Kind *)
  | Type  of loc                        (* Type *)
  | Var   of loc*Var.t                  (* Variable *)
  | Const of loc*ident*ident            (* Global variable *)
  | App   of term * term * term list    (* f a1 [ a2 ; ... an ] , f not an App *)
  | Lam   of loc*Var.t*term*term        (* Lambda abstraction *)
  | Pi    of loc*Var.t option*term*term (* Pi abstraction *)
  | Meta  of loc*int

let rec get_loc = function
  | Kind -> dloc
  | Type l | Var (l,_) | Const (l,_,_)
  | Lam (l,_,_,_) | Pi (l,_,_,_) | Meta (l,_) -> l
  | App (f,_,_) -> get_loc f

let mk_Kind             = Kind
let mk_Type l           = Type l
let mk_Var l v          = Var (l,v)
let mk_Const l m v      = Const (l,m,v)
let mk_Lam l x a b      = Lam (l,x,a,b)
let mk_Pi l x a b       = Pi (l,x,a,b)
let mk_Meta l n         = Meta (l,n)

let mk_App f a1 args =
  match f with
    | App (f',a1',args') -> App (f',a1',args'@(a1::args))
    | _ -> App(f,a1,args)

let mk_App_l f args = match args with
  | [] -> f
  | a::args' -> mk_App f a args'

let cpt = ref (-1)
let mk_Unique _ =
  incr cpt ;
  Const ( dloc , empty , hstring (string_of_int !cpt) )

(* [v_as_term] is [Var v]. This function returns the image of [v] in [subst]. *)
let rec subst_deref_rec subst v_as_term v =
  match subst_find subst v with
  | Some t ->
    begin match t with
    | Var (_,v') -> subst_deref_rec subst t v'
    | _ -> t
    end
  | None -> v_as_term

let subst_deref subst t = match t with
  | Var (_,v) -> subst_deref_rec subst t v
  | _ -> t

let term_eq t1 t2 =
  (* map is a mapping from bound vars of t1 to bound vars of t2, for alpha-equivalence *)
  let rec aux map t1 t2 = match subst_deref map t1, t2 with
    | Kind, Kind | Type _, Type _       -> true
    | Var (_,v1), Var (_,v2)            -> Var.equal v1 v2
    | Meta (_,n), Meta (_,n')           -> n=n'
    | Const (_,m,v), Const (_,m',v')    -> ident_eq v v' && ident_eq m m'
    | App (f,a,l), App (f',a',l')       ->
        aux map f f'
        && aux map a a'
        && aux_l map l l'
    | Lam (_,v,a,b), Lam (_,v',a',b')   ->
        let map' = VarMap.add v (mk_Var dloc v') map in
        aux map a a' && aux map' b b'
    | Pi (_,v_opt,a,b), Pi (_,v'_opt,a',b')   ->
        let map' = match v_opt, v'_opt with
          | Some v, Some v' -> VarMap.add v (mk_Var dloc v') map
          | _ -> map
        in
        aux map a a' && aux map' b b'
    | _, _  -> false
  and aux_l map l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | t1::tail1, t2::tail2 -> aux map t1 t2 && aux_l map tail1 tail2
  in
  aux VarMap.empty t1 t2

type pattern =
  | Var         of loc*Var.t
  | Pattern     of loc*ident*ident*pattern list
  | Brackets    of term
  | Joker       of loc*Var.t

type top = ident*pattern array

(* Context for type inference *)
type context = {
  var2ty : term subst;
  const2ty : term IdentMap.t;
  ident2var : Var.t IdentMap.t; (* scoping *)
}

let ctx_empty = {
  var2ty=subst_empty;
  const2ty=IdentMap.empty;
  ident2var=IdentMap.empty;
}

let ctx_bind ctx v ty = { ctx with var2ty=subst_bind ctx.var2ty v ty ; }

let ctx_declare ctx m ty =
  { ctx with const2ty = IdentMap.add m ty ctx.const2ty }

let ctx_bind_ident ctx m var =
  { ctx with ident2var = IdentMap.add m var ctx.ident2var }

(**{2 Rewrite Rules} *)

type rule = {
        l:loc;
        ctx:context;
        md:ident;
        id:ident;
        args:pattern list;
        rhs:term; }

type rule_constraint =
  | EqTerm of term * term

(** How to build a substitution, given a decision tree matching stack *)
type rule_subst_builder = (int * Var.t) list

(* decision tree for matching *)
type dtree =
  | Switch      of int * (int*ident*ident*dtree) list * dtree option
  | Test        of rule_subst_builder * rule_constraint list * term * dtree option

(** {2 Environment} *)

module H = Hashtbl.Make(
struct
  type t        = ident
  let equal     = ident_eq
  let hash      = Hashtbl.hash
end )

type rw_infos =
  | Decl    of term
  | Def     of term*term
  | Decl_rw of term*rule list*int*dtree

(** {2 Commands} *)

type command =
  (* Reduction *)
  | Whnf of preterm
  | Hnf of preterm
  | Snf of preterm
  | OneStep of preterm
  | Conv of preterm*preterm
  (*Tying*)
  | Check of preterm*preterm
  | Infer of preterm
  (* Misc *)
  | Gdt of ident*ident
  | Print of string
  | Other of string*preterm list
