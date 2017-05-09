open Basic
open Pp

let print_opentheory = ref false

let is_cst ty =
  match ty with
  | Term.App (cst,_,_) ->
    begin
      match cst with
      | Term.Const(_,md,id) -> string_of_ident id = "eta" && string_of_ident md = "hol"
      | _ -> false
    end
  | _ -> false

let is_proof ty =
  match ty with
  | Term.App (cst,_,_) ->
    begin
      match cst with
      | Term.Const(_,md,id) -> string_of_ident id = "eps" && string_of_ident md = "hol"
      | _ -> false
    end
  | _ -> false

(* eta or eps *)
let extract_type ty =
  match ty with
  | Term.App(_,ty,_) -> ty
  | _ -> assert false

let print_equality id ty t =
  let name = "eq_"^(string_of_ident id) in
  Format.printf "@[<2>%s : hol.eps (hol.leibniz (%a) %a (%a)).@]@.@." name print_term ty print_ident id print_term t;
  (* TODO: should be a theorem, but who cares ? *)
  let name_sym = "sym_eq_"^(string_of_ident id) in
  Format.printf "@[<2>%s : hol.eps (hol.leibniz (%a) (%a) %a).@]@.@." name_sym print_term ty print_term t print_ident id

let counter = ref 0

let db () = (Basic.hstring ("tr"^(string_of_int !counter)))

let rec extract_context' k left right : Term.term * (ident * ident) option =
  let fold (tl,tr) (ts,r) =
    match r with
    | None ->
      let t', r = extract_context' k tl tr in
      t'::ts,r
    | Some _ -> tl::ts,r
  in
  (*  Format.printf "debug:%a@.debug:%a@." print_term left print_term right; *)
  match (left,right) with
  | Term.Const(_,md,id), _ when not(Term.term_eq left right) ->
    Term.mk_DB dloc (db ()) k, Some (md,id)
  | Term.App(f,a,args), Term.App(f',a',args') ->
    let f,r = extract_context' k f f' in
    begin
      match r with
      | None -> let a,r = extract_context' k a a' in
        begin
          match r with
          | None ->
            let args,r = List.fold_right fold (List.combine args args') ([], None) in
            Term.mk_App f a args,r
          | Some _ -> Term.mk_App f a args, r
        end
      | Some _ -> Term.mk_App f a args, r
    end
  | Term.Pi(l,id,ty,te), Term.Pi(_,_,ty',te') ->
    let ty,r = extract_context' k ty ty' in
    begin
      match r with
      | None ->
        let te,r = extract_context' k te te' in
        Term.mk_Pi l id ty te,r
      | Some _ -> Term.mk_Pi l id ty te, r
    end
  | Term.Lam(l,id, Some ty, te), Term.Lam(_,_, Some ty', te') ->
    let ty,r = extract_context' k ty ty' in
    begin
      match r with
      | None ->
        let te,r = extract_context' (k+1) te te' in
        Term.mk_Lam l id (Some ty) te,r
      | Some _ -> Term.mk_Lam l id (Some ty) te,r
    end
  | Term.Lam(l,id, None, te), Term.Lam(_,_, None, te') ->
    let te,r = extract_context' (k+1) te te' in
    (*
    Format.printf "lambda: %a@." print_term te;
    Format.printf "lambda: %a@." print_term (Term.mk_Lam l id None te); *)
    Term.mk_Lam l id None te,r
  | _ -> left,None

let extract_context left right =
(*  Format.printf "debug1:%a@." print_term left;
  Format.printf "debug2:%a@." print_term right;
    Format.printf "%b@." (Term.term_eq left right); *)
  incr counter;
  let t,r = extract_context' 0 left right in
  (*  Format.printf "debug3:%a@." print_term t; *)
  match r with
  | None -> assert false
  | Some (md,id) ->
    match Env.get_type dloc md id with
    | OK ty ->
      db(), t, ty, (md,id)
    | Err e -> assert false



let rec plug db holet t =
    match holet with
    | Term.DB(_, id,_) when id = db -> t
    | Term.Lam(l,id,Some ty, te) ->
      Term.mk_Lam l id  (Some(plug db ty t)) (plug db te t)
    | Term.App(f,a,args) ->
      Term.mk_App (plug db f t) (plug db a t) (List.map (fun x -> plug db x t) args)
    | Term.Pi(l,id, ty, te) ->
      Term.mk_Pi l id (plug db ty t) (plug db te t)
    | _ -> holet


let do_only_delta () =
  Reduction.beta := false;
  Reduction.select (Some (fun r ->
      match r with
      | Rule.Delta _ -> true
      | _ -> false))

let undo_only_delta () =
  Reduction.beta := true;
  Reduction.select None

let leibnized ty_d t_d =
  let to_const md id =
    let name = "sym_eq_"^(string_of_ident id) in
    Term.mk_Const dloc md (hstring name)
  in
  let rec aux ty ctx =
    let ty = extract_type ty in
    (* Format.printf "before: %a@." print_term ty; *)
    do_only_delta ();
    let ty' = Reduction.one_step (Env.get_signature ()) ty in
    (* Format.printf "after: %a@." print_term ty'; *)
    undo_only_delta ();
    if Term.term_eq ty ty' then
      t_d
    else
      begin
        begin
          match Tracer.compare_terms ty ty' with
          | None -> failwith "error"
          | Some ctx -> Format.printf "ctx: %a@." print_term ctx.Tracer.term
        end;
        let db',tyle,ty, (md,id) = extract_context ty ty' in
        let tyle' =
          match ctx with
          | None -> tyle
          | Some(db,holet) ->
            plug db holet tyle
        in
        let t' = aux ty' (Some (db',tyle')) in
        let ctx = Term.mk_Lam dloc db' (Some ty) tyle' in
        (Term.mk_App (to_const md id) ctx [t'])
      end
  in
  aux ty_d None

let leibnized_term t = Term.(
    let shift n l =
      List.map (fun (i,(b,t)) -> (i+n,(b,t))) l
    in
    let rec adjust_db k t =
      match t with
      | App(f,a,[DB(l,id,0)]) -> mk_App f a [mk_DB l id k]
      | App(f,a,[t]) -> adjust_db k t
      | _ -> assert false
    in
    let rec leibnized_term dict t =
      match t with
      | Kind
      | Type _
      | Const _ -> t
      | DB(loc,id,n) ->
        let b,ty = (List.assoc n dict) in
        if b then
          adjust_db n ty
        else
          ty
      | App(f,a,args) -> mk_App (leibnized_term dict f) (leibnized_term dict a) (List.map (leibnized_term dict) args)
      | Lam(loc,id, Some ty, te) ->
        if is_cst ty || is_proof ty then
          let var = mk_DB dloc id 0 in
          let tle = leibnized ty var in
          let b = not (term_eq tle var) in
          let dict' = (0,(b,tle))::(shift 1 dict) in
          let te' = leibnized_term dict' te in
          mk_Lam loc id (Some ty) te'
        else
          mk_Lam loc id (Some ty) (leibnized_term ((0,(false,ty))::(shift 1 dict)) te)
      | _ -> assert false
    in
    leibnized_term [] t
)

module T = struct
  let mk_prelude _ i =
    Env.init i;
    Format.printf "#NAME %a.@.@." print_ident i

  let mk_declaration lc id st ty =
    let st_str = match st with
      | Signature.Static -> ""
      | Signature.Definable -> "def "
      | Signature.Injective -> "inj "
    in
    match Env.declare lc id st ty with
    | OK () -> Format.printf "@[<2>%s%a :@ %a.@]@.@." st_str print_ident id print_term ty
    | Err e -> Errors.fail_env_error e

  let mk_definition lc i ty t =
    match ty with
    | None -> Format.printf "@[<hv2>def %a@ :=@ %a.@]@.@." print_ident i print_term t
    | Some ty ->
      match Env.define lc i t (Some ty) with
      | OK () ->
        if is_cst ty then
          if !print_opentheory then
            begin
              Format.printf "@[<2>%a :@ %a.@]@.@." print_ident i print_term ty;
              Tracer.print_equality i (extract_type ty) t
            end
          else
            Format.printf "@[<hv2>def %a@ :=@ %a.@]@.@." print_ident i print_term t
        else
          let t' = Tracer.leibnize_term t in
          let t'' = Tracer.leibnize Tracer.Fold t' ty in
          Format.printf "@[<hv2>def %a :@ %a@ :=@ %a.@]@.@."
            print_ident i print_term ty print_term t''
      | Err e -> Errors.fail_env_error e

  let mk_opaque _ i ty t = match ty with
    | None -> Format.printf "@[<hv2>{%a}@ :=@ %a.@]@.@." print_ident i print_term t
    | Some ty ->
        Format.printf "@[<hv2>{%a}@ :@ %a :=@ %a.@]@.@."
          print_ident i print_term ty print_term t

  let mk_rules l =
    match Env.add_rules l with
    | OK _ ->  Format.printf "@[<v0>%a@].@.@." (print_list "" print_untyped_rule) l
    | Err err -> Errors.fail_env_error err

  let mk_command _ cmd =
    Format.printf "@[<2>%a@]@.@." Cmd.print_command cmd

  let mk_ending _ = ()
end

module P = Parser.Make(T)

let parse lb =
  try
    P.prelude Lexer.token lb ;
    while true do P.line Lexer.token lb done
  with
    | Tokens.EndOfFile -> ()
    | P.Error       -> Errors.fail (Lexer.get_loc lb)
                         "Unexpected token '%s'." (Lexing.lexeme lb)

let process_chan ic = parse (Lexing.from_channel ic)
let process_file name =
  (* Print.debug "Processing file %s\n" name; *)
  let ic = open_in name in
  process_chan ic;
  close_in ic

let from_stdin = ref false
let files = ref []
let add_file f = files := f :: !files

let options =
  [ "-stdin", Arg.Set from_stdin, " read from stdin";
    "-delta", Arg.Set Tracer.print_delta_trace , "delta trace";
    "-beta", Arg.Set Tracer.print_beta_trace , "beta trace";
    "-ot", Arg.Set  print_opentheory, "print is formated for open theory";
  ]

let  _ =
  Arg.parse options add_file "usage: dkindent file [file...]";
  if !from_stdin
    then process_chan stdin
    else List.iter process_file (List.rev !files)
