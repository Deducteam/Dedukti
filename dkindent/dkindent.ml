open Basic
open Parser
open Pp

let handle_entry e =
  let open Format in
  match e with
  | Decl(_,id,stat,ty)      ->
      let stat = if stat = Signature.Definable then "def " else "" in
      printf "@[<2>%s%a :@ %a.@]@.@." stat print_ident id print_term ty
  | Def(_,id,opaque,ty,te)  ->
      let key = if opaque then "thm" else "def" in
      begin
        match ty with
        | None    -> printf "@[<hv2>%s %a@ :=@ %a.@]@.@." key
                       print_ident id print_term te
        | Some ty -> printf "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." key
                       print_ident id print_term ty print_term te
      end
  | Rules(rs)               ->
      printf "@[<v0>%a@].@.@." (print_list "" print_untyped_rule) rs
  | Eval(_,cfg,te)          ->
      printf "#EVAL%a %a.@." print_red_cfg cfg print_term te
  | Infer(_,cfg,te)         ->
      printf "#INFER%a %a.@." print_red_cfg cfg print_term te
  | Check(_,assrt,neg,test) ->
      let cmd = if assrt then "#ASSERT" else "#CHECK" in
      let neg = if neg then "NOT" else "" in
      begin
        match test with
        | Convert(t1,t2) ->
            printf "%s%s %a ==@ %a.@." cmd neg print_term t1 print_term t2
        | HasType(te,ty) ->
            printf "%s%s %a ::@ %a.@." cmd neg print_term te print_term ty
      end
  | DTree(_,m,v)            ->
      begin
        match m with
        | None   -> printf "#GDT %a.@." print_ident v
        | Some m -> printf "#GDT %a.%a.@." print_mident m print_ident v
      end
  | Print(_, str)           ->
      printf "#PRINT %S.@." str

let  _ =
  (* Parsing of command line arguments. *)
  let from_stdin = ref None in
  let options =
    [ ( "--from-stdin"
      , Arg.String (fun name -> from_stdin := Some name)
      , "MODNAME parses from standard with the given module name" )
    ; ( "--default-rule"
      , Arg.Set print_default
      , " print a default rule name when none is given" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  (* Handle files. *)
  let handle_file fname =
    let ic = open_in fname in
    let md = Basic.mk_mident fname in
    Parser.handle_channel md handle_entry ic;
    close_in ic
  in
  List.iter handle_file files;
  (* Handle stdin. *)
  match !from_stdin with
  | None      -> ()
  | Some name -> Parser.handle_channel (mk_mident name) handle_entry stdin
