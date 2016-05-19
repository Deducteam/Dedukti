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
  val name        : Basics.ident ref
  val scope_term : Term.context -> Preterm.preterm -> Term.term m
  val scope_rule : Preterm.prule -> Rule.rule m
end

module Make(V:Visitor) = struct

  type 'a m = 'a V.m
    
  let name = ref qmark

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
<<<<<<< Updated upstream

=======
>>>>>>> Stashed changes


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

let p_of_pp (ctx:ident list) : prepattern -> (pattern m) =
  let rec aux k ctx = function
    | PPattern (l,None,id,pargs) ->
        let args = mmap (aux k ctx) pargs in
        ( match get_db_index ctx id with
            | Some n -> V.bind args (fun args' -> V.return (Var (l,id,n,args')))
            | None -> V.bind args (fun args' -> V.return (Pattern (l,!name,id,args')))
        )
    | PPattern (l,Some md,id,args) -> 
      let args = mmap (aux k ctx) args in
      V.bind args (fun args' -> V.return (Pattern (l,md,id, args')))
    | PLambda (l,x,p) -> V.bind (aux (k+1) (x::ctx) p) (fun p' ->      
      V.return (Lambda (l,x,p')))
    | PCondition pte -> V.bind (t_of_pt ctx pte) (fun ty -> V.return (Brackets ty))
    | PJoker l -> Errors.fail l "Unimplemeted feature '_'."
  in aux 0 ctx

let scope_pattern (ctx:context) (pp:prepattern) : pattern m =
  p_of_pp (List.map (fun (_,x,_) -> x) ctx) pp

(******************************************************************************)

let scope_context pctx = 
  let aux ctx0 (l,x,ty) = V.bind (ctx0) (fun ctx0 -> V.bind (scope_term ctx0 ty) 
    (fun ty' -> V.return ((l,x,ty')::ctx0))) in
  mfold aux [] pctx
(*
  let aux ctx0 (l,x,ty) = V.bind (scope_term ctx0 ty) (fun ty' -> (V.return (l,x,ty'))::ctx0) in
    List.fold_left aux [] pctx *)

let scope_rule (l,pctx,id,pargs,pri) =
  V.bind (scope_context pctx) (fun ctx ->
    V.bind (scope_pattern ctx (PPattern(l,None,id,pargs))) (fun pat ->
      V.bind (scope_term ctx pri) (fun ri -> V.return (ctx,pat,ri)))) (*
  let ctx = scope_context pctx in
  let pat = scope_pattern ctx (PPattern(l,None,id,pargs)) in
  let ri = scope_term ctx pri in
    (ctx,pat,ri)
							     *)
end
