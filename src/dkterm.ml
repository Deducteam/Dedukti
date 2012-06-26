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
| DDot of dkterm

type statement =
| Declaration of qid * dkterm
| RuleSet of ((qid * dkterm) list * dkterm * dkterm) list
| End

let rec subst v t = function
    DVar(Id w) when v = w -> t
  | DPi(Id w, ty, te) when v = w ->
      DPi(Id w, subst v t ty, te)
  | DPi(i, ty, te) ->
      DPi(i, subst v t ty, subst v t te)
  | DFun(Id w, ty, te) when v = w ->
      DFun(Id w, subst v t ty, te)
  | DFun(i, ty, te) ->
      DFun(i, subst v t ty, subst v t te)
  | DApp(t1,t2) -> DApp(subst v t t1, subst v t t2)
  | DDot(te) -> DDot(subst v t te)
  | t -> t


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

  method output_module out_chan prog = msgnl_with (self#with_ft out_chan) (prlist_with_sep fnl self#pr_statement prog)
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
    | DDot(t) -> str "{} " ++ self#pr_dkterm t

  method private pr_env = function
      [] -> str "[] "
    | b::q -> str "," ++ self#pr_binding b ++ spc () ++ self#pr_env q

  method private pr_rule (env, lhs, rhs) =
    self#rule_arr () ++ self#pr_env env ++ self#pr_dkterm lhs ++ spc () ++ self#pr_dkterm rhs

  method private pr_statement' = function
    | Declaration (n, t) ->
	str ":" ++ spc () ++ self#pr_qid n ++ spc () ++ self#pr_dkterm t
    | RuleSet rs -> prlist_with_sep fnl self#pr_rule rs
    | End -> mt ()

  method pr_statement t = hov 2 (self#pr_statement' t)

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
	surround (self#pr_qid n ++ pr_colon () ++ self#pr_dkterm t1
		    ++ spc ()
		  ++ self#pi_arr () ++ spc () ++ self#pr_dkterm t2)
    | DPi (n,t1,t2) ->
	surround (self#pr_qid n ++ pr_colon () ++ self#pr_dkterm t1
		  ++ spc () ++ self#pi_arr () ++ spc () ++ self#pr_dkterm t2)
    | DFun (n,t1,t2) ->
	surround (self#pr_qid n ++ pr_colon () ++ self#pr_dkterm t1
		  ++ spc () ++ self#fun_arr () ++ spc () ++ self#pr_dkterm t2)
    | DApp (t1,t2) -> surround (self#pr_dkterm t1 ++ spc () ++
				  self#pr_dkterm' t2)
    | DDot(t) -> hov 1 (str "{" ++ self#pr_dkterm' t ++ str "}")

  method pr_binding (n, t) = self#pr_qid n ++ pr_colon () ++ self#pr_dkterm t

  method private pr_rule (env, lhs, rhs) =
    let rec sep pp env = match env with
      | [] -> str ""
      | [n, t] -> pp ++ self#pr_binding (n, t)
      | (n, t) :: env' -> sep (pp ++ self#pr_binding (n, t) ++ pr_comma ()) env'
    in surround_brackets (sep (str "") env) ++ spc () ++
         self#pr_dkterm lhs ++ spc () ++ self#rule_arr () ++ spc () ++
         self#pr_dkterm rhs

  method pr_statement = function
    | Declaration (n, t) -> self#pr_binding (n, t) ++ str "."
    | RuleSet rs ->
        if rs = [] then mt () else
        prlist_with_sep fnl self#pr_rule rs ++ str "."
    | End -> mt ()
end

let pp_obj = ref new prefix_pp

let pp_prefix () = pp_obj := new prefix_pp

let pp_external () = pp_obj := new external_pp

let output_module out_chan prog = !pp_obj#output_module out_chan prog
