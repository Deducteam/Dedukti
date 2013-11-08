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
  Global.name := prelude_name;
  name := string_of_ident prelude_name

let mk_require = add_dep

let rec mk_term = function
  | GVar (lc, module_name, _) -> add_dep lc module_name
  | App l -> List.iter mk_term l
  | Lam (_, _, t1, t2)
  | Pi (_, _, t1, t2) -> mk_term t1 ; mk_term t2
  | _ -> ()

let mk_declaration _ _ t = mk_term t

let mk_definition _ _ = function
  | None -> mk_term
  | Some t -> mk_term t; mk_term

let mk_opaque = mk_definition

let mk_binding (_, _, t) = mk_term t 

let mk_ctx = List.iter mk_binding

let mk_pattern = function
  | Pattern ((lc, module_name, _), _) -> add_dep lc module_name
  | _ -> ()

let mk_top (_, parr) = Array.iter mk_pattern parr

let mk_rule (ctx, top, t) =
  mk_ctx ctx; mk_top top; mk_term t 

let mk_rules = List.iter mk_rule

let mk_ending () = Global.sprint (!name ^ ".dko : " ^ String.concat " " (List.map (fun s -> s ^ ".dko") !deps))
