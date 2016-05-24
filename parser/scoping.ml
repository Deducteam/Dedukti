open Basics
open Preterm
open Term
open Rule

module type Visitor = sig
   type 'a m
    type entry
    val return         : 'a -> 'a m
    val bind           : 'a m -> ('a -> 'b m) -> 'b m
    val mk_prelude     : Basics.loc -> Basics.ident -> entry m
    val mk_declaration : Basics.loc -> Basics.ident -> Term.term -> entry m
    val mk_definition  : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> entry m
    val mk_opaque      : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> entry m
    val mk_rules       : Rule.rule list -> entry m
    val mk_command     : Basics.loc -> Cmd.command -> entry m
    val mk_Type        : Basics.loc -> Term.term m
    val mk_DB          : Basics.loc -> Basics.ident -> int -> Term.term m
    val mk_Const       : Basics.loc -> Basics.ident -> Basics.ident -> Term.term m
    val mk_Lam         : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> Term.term m
    val mk_App         : Term.term  -> Term.term -> Term.term list -> Term.term m
    val mk_Pi          : Basics.loc -> Basics.ident -> Term.term -> Term.term -> Term.term m
    val mk_Arrow       : Basics.loc -> Term.term   -> Term.term -> Term.term m
    val mk_ending      : entry m -> unit
end

module type S = sig
  type 'a m
  val scope_term : Term.context -> Preterm.preterm -> Term.term m
  val scope_rule : Preterm.prule -> Rule.rule m
end

  let name = ref qmark

module Make(V:Visitor) = struct

  type 'a m = 'a V.m
    

  let get_db_index ctx id =
    let rec aux n = function
      | [] -> None
      | x::_ when (ident_eq id x) -> Some n
      | _::lst -> aux (n+1) lst
    in aux 0 ctx


  let empty = hstring ""
    
  let rec t_of_pt (ctx:ident list) (pte:preterm) : Term.term m =
    match pte with
    | PreType l    -> V.mk_Type l
    | PreId (l,id) ->
        begin
          match get_db_index ctx id with
            | None   -> V.mk_Const l !name id
            | Some n -> V.mk_DB l id n
        end
    | PreQId (l,md,id) -> V.mk_Const l md id
    | PreApp (f,a,args) -> (* type this is a bit tricky but there is only one way out *)
      V.bind (t_of_pt ctx f) (fun f -> 
	V.bind (t_of_pt ctx a) (fun a -> 
	  List.fold_left (fun app arg -> V.bind arg (fun x -> (V.bind app (fun ap -> V.mk_App ap  x [])))) (V.mk_App f a []) (List.map (t_of_pt ctx) args)))	    
    | PrePi (l,None,a,b) -> 
      V.bind (t_of_pt ctx a) (fun a ->
	V.bind (t_of_pt (empty::ctx) b) (fun b ->
	  V.mk_Arrow l a b))
    | PrePi (l,Some x,a,b) -> 
      V.bind (t_of_pt ctx a) (fun a ->
	V.bind (t_of_pt (x::ctx) b) (fun b ->
	  V.mk_Pi l x a b))
    | PreLam  (l,id,None,b) -> 
      V.bind (t_of_pt (id::ctx) b) (fun b -> V.mk_Lam l id None b)
    | PreLam  (l,id,Some a,b) ->
      V.bind (t_of_pt (id::ctx) b) (fun b -> 
	V.bind (t_of_pt ctx a) (fun a ->
	V.mk_Lam l id (Some a) b))



let scope_term (ctx:context) (pte:preterm) : Term.term m =
  t_of_pt (List.map (fun (_,x,_) -> x) ctx) pte

(******************************************************************************)

let rec mmap (f:'a -> 'b m) l = 
  match l with
  | [] -> V.return []
  | a::at -> V.bind (f a) (fun b -> V.bind (mmap f at) (fun bt -> V.return (b::bt)))

let rec mfold (f: 'b m -> 'a -> 'b m) (a:'b) (l:'a list)  =
  match l with
  | [] -> V.return a
  | x::t -> f (mfold f a t) x 

(* [get_vars_order vars p] traverses the pattern [p] from left to right and
 * builds the list of variables, taking jokers as variables. *)
let get_vars_order (vars:pcontext) (ppat:prepattern) : (loc*ident) list =
  let nb_jokers = ref 0 in
  let get_fresh_name () =
    incr nb_jokers;
    hstring ("?_" ^ string_of_int !nb_jokers)
  in
  let is_a_var id1 =
    let rec aux = function
      | [] -> None
      | (l,id2)::lst when ident_eq id1 id2 -> Some l
      | _::lst -> aux lst
    in aux vars
  in
  let rec aux (bvar:ident list) (ctx:(loc*ident) list) : prepattern -> (loc*ident) list = function
    | PPattern (_,None,id,pargs) ->
      begin
        if List.exists (ident_eq id) bvar then
          List.fold_left (aux bvar) ctx pargs
        else
          match is_a_var id with
          | Some l ->
            if List.exists (fun (_,a) -> ident_eq id a) ctx then
              List.fold_left (aux bvar) ctx pargs
            else
              let ctx2 = (l,id)::ctx in
              List.fold_left (aux bvar) ctx2 pargs
          | None -> List.fold_left (aux bvar) ctx pargs
      end
    | PPattern (l,Some md,id,pargs) -> List.fold_left (aux bvar) ctx pargs
    | PLambda (l,x,pp) -> aux (x::bvar) ctx pp
    | PCondition _ -> ctx
    | PJoker _ -> (dloc,get_fresh_name ())::ctx
  in
  aux [] [] ppat

let p_of_pp (ctx:ident list) (ppat:prepattern) : (pattern m) =
  let nb_jokers = ref 0 in
  let get_fresh_name () =
    incr nb_jokers;
    hstring ("?_" ^ string_of_int !nb_jokers)
  in
  let rec aux (ctx:ident list): prepattern -> (pattern m) = function
    | PPattern (l,None,id,pargs) ->
      let args = mmap (aux ctx) pargs in
      begin
        match get_db_index ctx id with
        | Some n -> V.bind args (fun args -> V.return (Var (l,id,n, args)))
        | None -> V.bind args (fun args -> V.return (Pattern (l,!name,id,args)))
      end
    | PPattern (l,Some md,id,pargs) -> 
      let args = mmap (aux ctx) pargs in
      V.bind args (fun args -> V.return (Pattern (l,md,id,args)))
    | PLambda (l,x,pp) -> V.bind (aux (x::ctx) pp) (fun pp -> V.return (Lambda (l,x,pp)))
    | PCondition pte -> V.bind (t_of_pt ctx pte) (fun pte -> V.return (Brackets pte))
    | PJoker l ->
      begin
        let id = get_fresh_name () in
        match get_db_index ctx id with
        | Some n -> V.return (Var (l,id,n,[]))
        | None -> assert false
      end
  in
  aux ctx ppat


let scope_pattern (ctx:context) (pp:prepattern) : pattern m =
  p_of_pp (List.map (fun (_,x,_) -> x) ctx) pp

(******************************************************************************)

let scope_rule (l,pctx,md_opt,id,pargs,pri:prule) : rule m =
  let top = PPattern(l,md_opt,id,pargs) in
  let ctx = get_vars_order pctx top in
  let idents = List.map snd ctx in
  V.bind (t_of_pt idents pri) (fun pri -> 
    V.bind (p_of_pp idents top) (fun top ->
    V.return ( ctx, top, pri )))

end
