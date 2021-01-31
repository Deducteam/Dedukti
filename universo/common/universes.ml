module B    = Kernel.Basic
module T    = Kernel.Term

type univ =
  | Sinf
  | Var of B.name
  | Enum of int


type pred =
  | Axiom of univ * univ
  | Cumul of univ * univ
  | Rule  of univ * univ * univ

type cstr = Pred of pred | EqVar of B.name * B.name

module C = Set.Make(struct type t = cstr let compare = compare end)

let md_theory = ref @@ B.mk_mident ""

let md_univ = ref (B.mk_mident "")

let pvar  () = B.mk_name !md_theory (B.mk_ident "var")

let sort  () = B.mk_name !md_theory (B.mk_ident "Sort")

let typ   () = B.mk_name !md_theory (B.mk_ident "type")

let set   () = B.mk_name !md_theory (B.mk_ident "set")

let prop  () = B.mk_name !md_theory (B.mk_ident "prop")

let univ  () = B.mk_name !md_theory (B.mk_ident "Univ")

let cast' () = B.mk_name !md_theory (B.mk_ident "cast'")

let axiom () = B.mk_name !md_theory (B.mk_ident "Axiom")

let rule  () = B.mk_name !md_theory (B.mk_ident "Rule")

let cumul () = B.mk_name !md_theory (B.mk_ident "Cumul")

let enum  () = B.mk_name !md_theory (B.mk_ident "enum")

let uzero () = B.mk_name !md_theory (B.mk_ident "uzero")

let usucc () = B.mk_name !md_theory (B.mk_ident "usucc")

let subtype () = B.mk_name !md_theory (B.mk_ident "SubType")

let forall () = B.mk_name !md_theory (B.mk_ident "forall")

let sinf () = B.mk_name !md_theory (B.mk_ident "sinf")

let true_ () = T.mk_Const B.dloc (B.mk_name !md_theory (B.mk_ident "true"))



let rec term_of_level i =
  assert (i>=0);
  if i = 0 then
    T.mk_Const B.dloc (uzero ())
  else
    T.mk_App (T.mk_Const B.dloc (usucc ())) (term_of_level (i-1)) []

let term_of_univ u =
  let lc = B.dloc in
  match u with
  | Sinf  -> T.mk_Const lc (sinf ())
  | Var n -> T.mk_Const lc n
  | Enum i -> T.mk_App (T.mk_Const B.dloc (enum ())) (term_of_level i) []

let term_of_pred p =
  let lc = B.dloc in
  match p with
  | Axiom(s,s') -> T.mk_App2 (T.mk_Const lc (axiom ()))
                     [term_of_univ s;term_of_univ s']
  | Cumul(s,s') -> T.mk_App2 (T.mk_Const lc (cumul ()))
                     [term_of_univ s;term_of_univ s']
  | Rule(s,s',s'') -> T.mk_App2 (T.mk_Const lc (rule ()))
                        [term_of_univ s; term_of_univ s'; term_of_univ s'']

let pattern_of_level _ = failwith "todo pattern of level"

let is_const cst t =
  match t with
  | T.Const(_,n) -> B.name_eq cst n
  | _ -> false


let is_var md_elab t =
  match t with
  | T.Const(_,n) -> B.md n = md_elab
  | _ -> false

let is_enum t =
  match t with
  | T.App(f,_,[]) when is_const (enum ()) f -> true
  | _ -> false

let is_subtype t =
  match t with
  | T.App(f,_,[_;_;_]) when is_const (subtype ()) f -> true
  | _ -> false

let extract_subtype t =
  match t with
  | T.App(f,_,[_;_;_]) as s when is_const (subtype ()) f -> s
  | _ -> assert false

let is_forall t =
  match t with
  | T.App(f,_,[_;_]) when is_const (forall ()) f -> true
  | _ -> false

let extract_forall t =
  match t with
  | T.App(f,_,[_;T.Lam(_,_,_,t)]) when is_const (forall ()) f -> t
  | _ -> assert false

let is_cast' t =
  match t with
  | T.App(f,_,_) when is_const (cast' ()) f -> true
  | _ -> false

let extract_cast' t =
  match t with
  | T.App(f,s1,[s2;a;b;t]) when is_const (cast' ()) f -> s1,s2,a,b,t
  | T.App(f,s1,s2::a::b::m::n) when is_const (cast' ()) f -> s1,s2,a,b,T.mk_App2 m n
  | _ -> Format.eprintf "%a@." Api.Pp.Default.print_term t; assert false

let rec extract_level : T.term -> int = fun t ->
  match t with
  | T.Const(_,n) when B.name_eq n (uzero ()) -> 0
  | T.App (T.Const(_,n),l,[]) when B.name_eq n (usucc ()) ->
    1 + (extract_level l)
  | _ -> assert false

exception Not_univ
exception Not_pred

let extract_univ : T.term -> univ = fun t ->
  match t with
  | T.Const(_,n) when n = sinf () -> Sinf
  | T.Const(_,n) -> Var n
  | T.App (T.Const(_,c),n,[]) when B.name_eq c (enum ()) ->
    Enum (extract_level n)
  | _ -> Format.eprintf "%a@." Api.Pp.Default.print_term t; assert false

let extract_pred t =
  match t with
  | T.App(f,s,[s'])     when is_const (axiom ()) f ->
    Axiom(extract_univ s, extract_univ s')
  | T.App(f,s,[s'])     when is_const (cumul ()) f ->
    Cumul(extract_univ s, extract_univ s')
  | T.App(f,s,[s';s'']) when is_const (rule ()) f ->
    Rule(extract_univ s, extract_univ s', extract_univ s'')
  | _ -> raise Not_pred
