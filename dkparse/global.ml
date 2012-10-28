open Types

let out  = ref stdout
let name = ref "anon"

(* true->local , false->global *)             
let gs : (id,bool) Hashtbl.t = Hashtbl.create 47

let mk_var (id,loc) =
  try 
    if Hashtbl.find gs id then Var id
    else GVar id
  with
    | Not_found -> raise (Error (ScopeError (id,loc))) 

let mk_evar qid = EVar qid      (*FIXME (qid = mod.id) vérifier que mod <> !name*)

let filter_qid qid = qid        (*FIXME idem *)

let mk_pat_var (id,loc) =
  try
    if Hashtbl.find gs id then Id id
    else Pat (id,[||],[||])
  with
    | Not_found -> raise (Error (ScopeError (id,loc))) 

let gscope_add (id,loc) = 
  if Hashtbl.mem gs id then raise (Error (AlreadyDefinedId (id,loc))) 
  else Hashtbl.add gs id false

let chk_rules_id (loc,(id,_)) = 
  if Hashtbl.mem gs id then () 
  else raise (Error (ScopeError (id,loc)))

let lscope_add id = Hashtbl.add gs id true

let lscope_remove id = Hashtbl.remove gs id

let lscope_remove_lst lst = List.iter (fun x -> lscope_remove (fst x)) lst
