open Term

module P = Parser.Make(Dep)

let parse' lb =
  let rec aux m =    
    try
      aux (Dep.bind m (fun _ -> (P.line Lexer.token lb)))
    with
    | Tokens.EndOfFile -> m
    | P.Error -> Errors.fail (Lexer.get_loc lb) "Unexpected token '%s'." (Lexing.lexeme lb)
  in
  try
    let m = P.prelude Lexer.token lb in
    let m' = aux m in
    Dep.mk_ending m'
  with
  | Tokens.EndOfFile -> ()
  | P.Error       -> Errors.fail (Lexer.get_loc lb)
    "Unexpected token '%s'." (Lexing.lexeme lb)


let run_on_file file =
  if !(Dep.verbose) then Printf.eprintf "Running Dkdep on file \"%s\".\n" file;
  flush stderr;
  let input = open_in file in
  Dep.filename := file;
    parse' (Lexing.from_channel input)

let args =
  [ ("-o", Arg.String (fun fi -> Dep.out := open_out fi), "Output file"  );
    ("-v", Arg.Set Dep.verbose, "Verbose mode" );
    ("-s", Arg.Set Dep.sorted, "Sort file with respect to their dependence")
  ]

let print_out fmt = Printf.kfprintf (fun _ -> output_string stdout "\n" ) stdout fmt
    
let _ =
  try
    Arg.parse args run_on_file "Usage: dkdep [options] files";
    if !Dep.sorted then
      let l = Dep.sort() in
      print_out "%s" (String.concat " " (List.map (fun s -> s ^ ".dk") l))
  with
    | Sys_error err             -> Printf.eprintf "ERROR %s.\n" err; exit 1
    | Exit                      -> exit 3
