open Types


(* Check for external symbols *)

let mchecked : (string list) ref = ref []

let is_checked m =
  if List.mem m !mchecked then false
  else ( mchecked := m::!mchecked ; true )

(* Term construction & Scoping *)

type vart = Local | Global

let gs : (string,vart) Hashtbl.t = Hashtbl.create 47

let clear _ = Hashtbl.clear gs

(*
let filter_qid (md,id,l) = 
  if md = !name then 
    begin
      try
        begin
          match Hashtbl.find gs id with 
            | Local   -> raise (ParserError (ScopeError ((md,id),l)))
            | _       -> (md,id)
        end
      with
        | Not_found -> raise (ParserError (ScopeError ((md,id),l)))
    end
  else  
      (*if List.mem md !libs then *)
        (md,id)
      (*else raise (ParsingError (UnknownModule (md,l)))*)


 *)

let gscope_add (v:var) =
  let id = snd v in 
    if Hashtbl.mem gs id then raise (ParserError (AlreadyDefinedId v)) 
    else 
      Hashtbl.add gs id Global 

let gscope_add_decl (v:var) = 
  if !Global.ignore_redeclarations then
    if Hashtbl.mem gs (snd v) then false
    else ( gscope_add v ; true )
  else
    ( gscope_add v ; true )

let lscope_add id = Hashtbl.add gs id Local

let lscope_remove id = Hashtbl.remove gs id

let lscope_remove_lst (lst:(var*pterm)list) : unit = List.iter (fun ((_,x),_) -> lscope_remove x) lst 


(* pterm to gterm *)

let rec check_scope : pterm -> term = function
  | P_Type                      -> Type
  | P_Id v                      ->
      begin
        try
          ( match Hashtbl.find gs (snd v) with 
              | Local   -> LVar v
              | _       -> GVar (fst v,!Global.name,snd v) )
        with
          | Not_found -> raise (ParserError (ScopeError v)) 
      end
  | P_Qid id                    -> 
      begin
        let (l,m,v) = id in
          if m = !Global.name then 
            try
              begin
                match Hashtbl.find gs v with 
                  | Local   -> raise (ParserError (ScopeError (l,v)))
                  | _       -> GVar (l,m,v)
              end
            with
              | Not_found -> raise (ParserError (ScopeError ((l,v))))
      else 
        GVar (l,m,v)
      end
  | P_App (f,a)                 -> App (check_scope f,check_scope a)
  | P_Lam (v,ty,te)             -> 
      let ty' = 
        match ty with
          | None -> None
          | Some ty0 -> Some (check_scope ty0)
      in
      let _   = lscope_add (snd v)      in
      let te' = check_scope te          in
      let _   = lscope_remove (snd v)   in
        Lam (v,ty',te')
  | P_Pi (Some v,a,b)           ->
      let a' = check_scope a            in
      let _  = lscope_add (snd v)       in
      let b' = check_scope b            in
      let _  = lscope_remove (snd v)    in
        Pi (Some v,a',b')
  | P_Pi (None,a,b)             -> Pi (None,check_scope a,check_scope b)

let rec check_scope_pat = function
  | Pat_Id v                    -> 
      begin
        try
          match Hashtbl.find gs (snd v) with
            | Local -> Var v
            | _     -> Pat ((fst v,!Global.name,snd v),[||],[||])
        with
          | Not_found -> raise (ParserError (ScopeError v)) 
      end
  | Pat_Pa (id,dots,pats)       -> Pat (id,Array.map check_scope dots,Array.map check_scope_pat pats)

let check_scope_rule (env,(id,dots,pats),te:prule) : rule = 
  let env'  = List.map (
    fun (v,t) -> 
      let t' = check_scope t in
        lscope_add (snd v) ; (v,t')
  ) env in
  let dots' = Array.map check_scope dots        in
  let pats' = Array.map check_scope_pat pats    in
  let te'   = check_scope te                    in
  let _     = lscope_remove_lst env             in
    (env',(id,dots',pats'),te')
  
let check_scope_rules (lst:prule list) : rule list = 
  match lst with
    | []                        -> assert false
    | (_,((l,id),_,_),_)::lst'       ->
        begin
            if not (Hashtbl.mem gs id) then raise (ParserError (ScopeError (l,id))) ;
            List.iter (
              fun (_,((l',id'),_,_),_) -> 
                if id<>id' then raise (ParserError (ConstructorMismatch ((l,id),(l',id'))))
            ) lst' ;
            List.map check_scope_rule lst 
        end

