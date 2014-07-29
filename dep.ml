open Types

let deps = ref []
let name = ref ""

let add_dep lc m =
  assert (not (ident_eq m empty));
  let s = string_of_ident m in
  if List.mem s (!name :: !deps) then ()
  else deps := List.sort compare (s :: !deps)

let mk_prelude _ prelude_name =
  deps := [];
  name := string_of_ident prelude_name

let rec mk_term = function
  | PreQId (lc, module_name, _) -> add_dep lc module_name
  | PreApp (f,a,args) -> (mk_term f ; mk_term a ; List.iter mk_term args )
  | PreLam (_, _, t1, t2)
  | PrePi (_,_, t1, t2) -> mk_term t1 ; mk_term t2
  | _ -> ()

let rec mk_pattern = function
  | PPattern (l,m_opt,_,args)   ->
      let _ = match m_opt with
        | None          -> ()
        | Some m        -> add_dep l m
      in List.iter mk_pattern args
  | _                           -> ()

let mk_declaration _ _ t = mk_term t

let mk_definition _ _ = function
  | None -> mk_term
  | Some t -> mk_term t; mk_term

let mk_opaque = mk_definition

let mk_binding (_, _, t) = mk_term t

let mk_ctx : pdecl list -> unit = List.iter mk_binding

let mk_prule (l,ctx,id,pats,ri:prule) =
  mk_ctx ctx; mk_pattern (PPattern (l,None,id,pats)); mk_term ri

let mk_rules = List.iter mk_prule

let mk_command _ = function
  | Whnf t | Hnf t | Snf t
  | OneStep t | Infer t                 -> mk_term t
  | Conv (t1,t2) | Check (t1,t2)        -> ( mk_term t1 ; mk_term t2 )
  | Gdt (_,_) | Print _                 -> ()
  | Other (_,lst)                       -> List.iter mk_term lst

let mk_ending () =
  Global.print_out "%s.dko : %s" !name
    (String.concat " " (List.map (fun s -> s ^ ".dko") !deps) )
