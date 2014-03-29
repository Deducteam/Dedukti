open Types

let linearity                   = ref true
let subject_reduction_criterion = ref true
let toyama_criterion            = ref true
let weak_confluence             = ref WCR_R

let modules : (ident,rw_infos H.t) Hashtbl.t = Hashtbl.create 19

(* -------------- *)

let get_type m v =
  try
    begin
      match H.find (Hashtbl.find modules m) v with
        | Decl ty | Def (_,ty) | Decl_rw (ty,_,_,_) -> ty
    end
  with Not_found -> assert false

(* -------------- *)

let rec rev_append_dko l1 l2 =
  match l1 with
    | [] -> l2
    | a::lst -> (a^".dko") :: (rev_append_dko lst l2)

let rec load = function
  | [] -> ()
  | file::lst ->
      let obj = Dko.unmarshal dloc file in
      let m = hstring obj.Dko.name in
        if Hashtbl.mem modules m then load lst
        else
          ( Hashtbl.add modules m obj.Dko.table ;
            load (rev_append_dko obj.Dko.dependencies lst) )

let iterator f = Hashtbl.iter (
  fun m env ->
    let aux v =
      function
        | Decl _ -> ()
        | Def _ -> () (*TODO def to rules2 *)
        | Decl_rw (_,rules,_,_) -> List.iter (f m v) rules
    in
      H.iter aux env
  )
    modules

(* -------------- *)

let check_cond_rw r =
  let rec aux = function
    | [] -> ()
    | (DB _,_)::lst | (_,DB _)::lst -> aux lst
    | _ -> failwith "Not implemented (Conditionnal Rewriting)."
  in
    aux r.constraints

let subject_reduction m v r =
  let rec is_kind = function
    | Type -> true
    | Pi (_,_,ty) -> is_kind ty
    | _ -> false
  in
  let type_level = is_kind (get_type m v) in
  let constant_applicative =
    match r.right with
      | App (Const (_,_)::lst) -> true
      | _ -> false
  in
    (not type_level || constant_applicative)

(* This is experimental... *)
let modularity_criterion r =
  (*TODO check arity*)
  match r.right with
    | Lam (_,_,_) -> false
    | App (Const (_,_)::lst) -> true
    | App (_) -> false
    | _ -> true

let is_linear r = r.constraints <> []

(* -------------- *)

let init dko =
  load [dko] ;
  iterator (
    fun m v r ->
      check_cond_rw r ;
      ( if not (is_linear r) then
          linearity := false );
      ( if not (subject_reduction m v r) then
          subject_reduction_criterion := false );
      ( if not (modularity_criterion r) then
          toyama_criterion := false )
  )
  (*TODO weak confluence*)

let export out =
  iterator (Tpdb.print_rule out)
