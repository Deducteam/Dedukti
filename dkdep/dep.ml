open Basics
open Term
open Rule
open Cmd

let out = ref stdout
let deps = ref []
let name = ref ""

let print_out fmt = Printf.kfprintf (fun _ -> output_string !out "\n" ) !out fmt

let add_dep m =
  let s = string_of_ident m in
  if List.mem s (!name :: !deps) then ()
  else deps := List.sort compare (s :: !deps)

let mk_prelude _ prelude_name =
  deps := [];
  name := string_of_ident prelude_name

let rec mk_term = function
  | Kind | Type _ | DB _ -> ()
  | Const (_,md,_) -> add_dep md
  | App (f,a,args) -> List.iter mk_term (f::a::args)
  | Lam (_,_,None,te) -> mk_term te
  | Lam (_,_,Some a,b)
  | Pi (_,_,a,b) -> ( mk_term a; mk_term b )


let rec mk_pattern = function
  | Var  (_,_,_,args) -> List.iter mk_pattern args
  | Pattern (_,md,_,args) -> ( add_dep md ; List.iter mk_pattern args )
  | Lambda (_,_,te) -> mk_pattern te
  | Brackets t -> mk_term t
  | Joker _ -> ()

let mk_declaration _ _ t = mk_term t

let mk_definition _ _ = function
  | None -> mk_term
  | Some t -> mk_term t; mk_term

let mk_opaque = mk_definition

let mk_binding ( _, t) = mk_term t

let mk_ctx = List.iter mk_binding

let mk_prule (ctx,pat,rhs) =
  mk_ctx ctx; mk_pattern pat; mk_term rhs

let mk_rules = List.iter mk_prule

let mk_command _ = function
  | Whnf t | Hnf t | Snf t
  | OneStep t | Infer t                 -> mk_term t
  | Conv (t1,t2) | Check (t1,t2)        -> ( mk_term t1 ; mk_term t2 )
  | Gdt (_,_) | Print _                 -> ()
  | Other (_,lst)                       -> List.iter mk_term lst

let mk_ending () =
  print_out "%s.dko : %s" !name
    (String.concat " " (List.map (fun s -> s ^ ".dko") !deps) )
