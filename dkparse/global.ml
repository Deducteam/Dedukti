open Types

let out                                 = ref stdout
let name                                = ref "anon"
let do_not_check                        = ref false
let quiet                               = ref false
let ignore_redeclarations               = ref false
let libs  :  string list ref            = ref []

let debug str  = if !quiet then () else ( prerr_string str ; flush stderr )
let debug_ok _ = if !quiet then () else prerr_endline "\027[32m[OK]\027[m"
let debug_ko _ = if !quiet then () else prerr_endline "\027[31m[KO]\027[m"

(* let get_gvar_name v = !name ^ "." ^ v *)

type tvar = | Local | Global | Alias

let gs : (string,tvar) Hashtbl.t = Hashtbl.create 47

let is_alias m id =
  if m <> !name then true
  else
    try 
      match Hashtbl.find gs id with
        | Alias   -> true
        | Global  -> false
        | Local   -> raise (InternalError (IsAlias1 (m,id)))
    with
      | Not_found -> raise (InternalError (IsAlias2 (m,id)))

let mk_var (id,loc) =
  try
   begin
     match Hashtbl.find gs id with 
       | Local   -> Var id
       | _       -> GVar (!name,id)
   end
  with
    | Not_found -> raise (ParsingError (ScopeError (id,loc))) 

let mk_evar (md,id,l) = 
  if md = !name then 
    begin
      try
        begin
          match Hashtbl.find gs id with 
            | Local   -> raise (ParsingError (ScopeError (md^"."^id,l)))
            | _       -> GVar (md,id)
        end
      with
        | Not_found -> raise (ParsingError (ScopeError (md^"."^id,l)))
    end
  else 
      if List.mem md !libs then GVar (md,id)
      else raise (ParsingError (UnknownModule (md,l)))

let filter_qid (md,id,l) = 
  if md = !name then 
    begin
      try
        begin
          match Hashtbl.find gs id with 
            | Local   -> raise (ParsingError (ScopeError (md^"."^id,l)))
            | _       -> (md,id)
        end
      with
        | Not_found -> raise (ParsingError (ScopeError (md^"."^id,l)))
    end
  else  
      if List.mem md !libs then (md,id)
      else raise (ParsingError (UnknownModule (md,l)))

let mk_pat_var (id,loc) =
  try
    match Hashtbl.find gs id with
      | Local -> Id id
      | _     -> Pat ((!name,id),[||],[||])
  with
    | Not_found -> raise (ParsingError (ScopeError (id,loc))) 

let gscope_add (id,loc) = 
  if Hashtbl.mem gs id then raise (ParsingError (AlreadyDefinedId (id,loc))) 
  else Hashtbl.add gs id Global 
  

let gscope_add_decl (id,loc) = 
  if Hashtbl.mem gs id then false
  else ( Hashtbl.add gs id Global ; true )

let chk_alias id rs =
 if Array.length rs !=1 then ()
 else
   let (_,_,a,b,_) = rs.(0) in
    if ( Array.length a = 0 && Array.length b = 0 ) then Hashtbl.replace gs id Alias
    else () 


let chk_rules_id (loc,((_,id),_)) = 
  if Hashtbl.mem gs id then ()
  else raise (ParsingError (ScopeError (id,loc)))

let lscope_add id = Hashtbl.add gs id Local

let lscope_remove id = Hashtbl.remove gs id

let lscope_remove_lst lst = List.iter (fun x -> lscope_remove (fst (fst x))) lst

