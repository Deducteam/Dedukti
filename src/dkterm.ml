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

class virtual base_pp  = object (self)

  method with_ft chan =
    with_fp { fp_output = chan;
	      fp_output_function = output chan;
	      fp_flush_function = fun _ -> flush chan }

  (* Some custom printer combinators for a few symbols. *)
  method fun_arr () = str "=>"
  method pi_arr () = str "->"
  method rule_arr () = str "--> "

  method pr_qid = function
    | Id s -> str s
    | Qid (path,s) -> str path ++ str "." ++ str s

  method virtual pr_dkterm : dkterm -> std_ppcmds

  method virtual pr_statement : statement -> std_ppcmds

  method output_term out_chan t = pp_with (self#with_ft out_chan) (self#pr_dkterm t)

  method output_module out_chan prog = pp_with (self#with_ft out_chan) (pr_vertical_list self#pr_statement prog)
end


class prefix_pp = object (self)
  inherit base_pp as super

  method pr_binding (n, t) = pr_colon () ++  self#pr_qid n ++ spc () ++ self#pr_dkterm t

  method pr_dkterm = function
    | DType -> str "Type"
    | DKind -> str "Kind"
    | DVar n -> self#pr_qid n
    | DPi(n, t1, t2) -> self#pi_arr () ++ self#pr_binding (n,t1) ++ spc () ++ self#pr_dkterm t2
    | DFun(n, t1,t2) -> self#fun_arr () ++ self#pr_binding (n,t1) ++ spc () ++ self#pr_dkterm t2
    | DApp(t1,t2) -> str "@" ++ spc () ++ self#pr_dkterm t1 ++ spc () ++ self#pr_dkterm t2

  method private pr_env = function
      [] -> str "[] "
    | b::q -> pr_coma () ++ self#pr_binding b ++ spc () ++ self#pr_env q

  method pr_statement = function
    | Declaration (n, t) -> self#pr_binding (n, t) 
    | Rule (env, lhs, rhs) ->
	self#rule_arr () ++ self#pr_env env ++ self#pr_dkterm lhs ++ spc () ++ self#pr_dkterm rhs
    | End -> mt ()
	
  method output_module out_chan prog = 
    let magic_string = "(; # FORMAT prefix # ;)" in
      msgnl_with (self#with_ft out_chan) (str magic_string);
      super#output_module out_chan prog
end

class external_pp = object (self)
  inherit base_pp

  method pr_dkterm = function
    | DApp(t1,t2) -> self#pr_dkterm t1 ++ spc () ++ self#pr_dkterm' t2
    | t -> self#pr_dkterm' t

  method private pr_dkterm' = function
    | DType -> str "Type"
    | DKind -> str "Kind"
    | DVar n -> self#pr_qid n
    | DPi (n, (DPi _ as t1), t2) ->
	surround (self#pr_qid n ++ pr_colon () ++ surround (self#pr_dkterm t1)
		  ++ self#pi_arr () ++ self#pr_dkterm t2)
    | DPi (n,t1,t2) ->
	surround (self#pr_qid n ++ pr_colon () ++ self#pr_dkterm t1
		  ++ spc () ++ self#pi_arr () ++ spc () ++ self#pr_dkterm t2)
    | DFun (n,t1,t2) ->
	surround (self#pr_qid n ++ pr_colon () ++ self#pr_dkterm t1 
		  ++ spc () ++ self#fun_arr () ++ spc () ++ self#pr_dkterm t2)
    | DApp (t1,t2) -> surround (self#pr_dkterm t1 ++ spc () ++ 
				  self#pr_dkterm' t2)

  method pr_binding (n, t) = self#pr_qid n ++ pr_colon () ++ self#pr_dkterm t

  method pr_statement = function
    | Declaration (n, t) -> self#pr_binding (n, t) ++ str "."
    | Rule (env, lhs, rhs) ->
	let rec sep pp env = match env with
	  | [] -> str ""
	  | [n, t] -> pp ++ self#pr_binding (n, t)
	  | (n, t) :: env' -> sep (pp ++ self#pr_binding (n, t) ++ pr_coma ()) env'
	in surround_brackets (sep (str "") env) ++ spc () ++
             self#pr_dkterm lhs ++ spc () ++ self#rule_arr () ++ spc () ++
	     self#pr_dkterm rhs ++ str "."
    | End -> mt ()
end
