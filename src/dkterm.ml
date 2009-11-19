(***** Dedukti Syntax ****)

type qid =
  | Id of string
  | Qid of string * string

type dkterm =
| DType
| DKind
| DVar of qid
| DPi of qid * dkterm * dkterm
| DFun of qid * dkterm * dkterm
| DApp of dkterm * dkterm

type line =
| Declaration of qid * dkterm
| Rule of (qid * dkterm) list * dkterm * dkterm
| End


(**** Prettyprinting ****)

let get_euname n = match n with
  | Id s -> s
  | Qid (path,s) -> path ^ "." ^ s

let rec ast_to_str t = match t with
  | DType -> "Type"
  | DKind -> "Kind"
  | DVar n -> get_euname n
  | DPi (n,t1,t2) -> "(" ^ (get_euname n) ^ " : " ^ (ast_app t1) ^ " -> " ^ (ast_app t2) ^ ")"
  | DFun (n,t1,t2) -> "(" ^ (get_euname n) ^ " : " ^ (ast_app t1) ^ " => " ^ (ast_app t2) ^ ")"
  | DApp (t1,t2) -> "(" ^ (ast_app t1) ^ " " ^ (ast_to_str t2) ^")"
and ast_app t = match t with
  | DApp(t1,t2) -> ast_app t1 ^ " " ^ ast_to_str t2
  | t -> ast_to_str t 

let pprint expres = Printf.printf "%s" (ast_to_str expres)

let output_term out_chan t = output_string out_chan (ast_app t)

let output_decl out_chan (i,t) =
  output_string out_chan (get_euname i);
  output_string out_chan " : ";
  output_term out_chan t

let rec output_decl_list out_chan = function
    [] -> ()
  | [d] -> output_decl out_chan d
  | d::q -> 
      output_decl out_chan d; 
      output_string out_chan ", ";
      output_decl_list out_chan q

let output_line out_chan = function
  | Declaration(i,t) -> output_decl out_chan (i,t);
      output_string out_chan ".\n"
  | Rule(var_decls, t1, t2) ->
      output_string out_chan "[";
      output_decl_list out_chan var_decls;
      output_string out_chan "] ";
      output_term out_chan t1;
      output_string out_chan " --> ";
      output_term out_chan t2;
      output_string out_chan ".\n"
  | End -> output_string out_chan "\n"

