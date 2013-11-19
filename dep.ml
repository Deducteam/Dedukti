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

let mk_require = add_dep

let rec mk_term = function
  | P_QId (lc, module_name, _) -> add_dep lc module_name
  | P_App l -> List.iter mk_term l
  | P_Lam (_, _, t1, t2)
  | P_Pi (_, t1, t2) -> mk_term t1 ; mk_term t2
  | _ -> ()

let mk_declaration _ _ t = mk_term t

let mk_definition _ _ = function
  | None -> mk_term
  | Some t -> mk_term t; mk_term

let mk_opaque = mk_definition

let mk_binding (_, _, t) = mk_term t 

let mk_ctx = List.iter mk_binding

let mk_prule (ctx, top, t) =
  mk_ctx ctx; mk_term top; mk_term t 

let mk_rules = List.iter mk_prule

let mk_assert _ _ _ = assert false (*FIXME*)

let mk_ending () = Global.sprint (!name ^ ".dko : " ^ String.concat " " (List.map (fun s -> s ^ ".dko") !deps))
