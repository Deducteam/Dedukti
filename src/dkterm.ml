open Pp
open Pp_control
open Util

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

type statement =
| Declaration of qid * dkterm
| Rule of (qid * dkterm) list * dkterm * dkterm
| End

(* Some custom printer combinators for a few symbols. *)
let fun_arr () = str " => "
let pi_arr () = str " -> "
let rule_arr () = str " --> "

let pr_qid = function
  | Id s -> str s
  | Qid (path,s) -> str path ++ str "." ++ str s

let rec pr_dkterm = function
  | DApp(t1,t2) -> pr_dkterm t1 ++ spc () ++ pr_dkterm' t2
  | t -> pr_dkterm' t
and pr_dkterm' = function
  | DType -> str "Type"
  | DKind -> str "Kind"
  | DVar n -> pr_qid n
  | DPi (n, (DPi _ as t1), t2) ->
      surround (pr_qid n ++ pr_colon () ++ surround (pr_dkterm t1) ++ pi_arr () ++ pr_dkterm t2)
  | DPi (n,t1,t2) ->
      surround (pr_qid n ++ pr_colon () ++ pr_dkterm t1 ++ pi_arr () ++ pr_dkterm t2)
  | DFun (n,t1,t2) ->
      surround (pr_qid n ++ pr_colon () ++ pr_dkterm t1 ++ fun_arr () ++ pr_dkterm t2)
  | DApp (t1,t2) -> surround (pr_dkterm t1 ++ spc () ++ pr_dkterm' t2)

let pr_binding (n, t) = pr_qid n ++ pr_colon () ++ pr_dkterm t

let pr_statement = function
  | Declaration (n, t) -> pr_binding (n, t) ++ str "."
  | Rule (env, lhs, rhs) ->
      let rec sep pp env = match env with
	| [] -> str ""
	| [n, t] -> pp ++ pr_binding (n, t)
	| (n, t) :: env' -> sep (pp ++ pr_binding (n, t) ++ pr_coma ()) env'
      in surround_brackets (sep (str "") env) ++ spc () ++
	   pr_dkterm lhs ++ rule_arr () ++ pr_dkterm rhs ++ str "."

let with_ft chan =
  with_fp { fp_output = chan;
	    fp_output_function = output chan;
	    fp_flush_function = fun _ -> flush chan }

let output_term out_chan t = pp_with (with_ft out_chan) (pr_dkterm t)

let output_module out_chan prog = pp_with (with_ft out_chan) (pr_vertical_list pr_statement prog)
