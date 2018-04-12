open Format
open Basic
open Term
open Rule

let pp_name fmt cst =
  fprintf fmt "%a_%a" pp_mident (md cst) pp_ident (id cst)

type confluence_error =
  | NotConfluent of string
  | MaybeConfluent of string
  | CCFailure of string

module IdMap = Map.Make(
    struct
      type t = ident
      let compare x y = String.compare (string_of_ident x) (string_of_ident y)
    end
    )

let confluence_command = ref ""
let file_out = ref None
let do_not_erase_confluence_file = ref false

let set_cmd cmd =
  ( if not (Sys.file_exists cmd) then
      raise (Sys_error ("'" ^ cmd ^ "' does not exist")));
  confluence_command := cmd

let initialize () =
  do_not_erase_confluence_file := false;
  if (String.compare !confluence_command "") == 0 then file_out := None
  else
    begin
      let (file,out) = Filename.open_temp_file "dkcheck" ".trs" in
      let fmt = formatter_of_out_channel out in
      debug 1 "Confluence temporary file:%s" file;
      file_out := (Some (file,out));
      fprintf fmt "\
(FUN
  lam : term -> (term -> term) -> term
  app : term -> term -> term
  pi  : term -> (term -> term) -> term
  type : term
)

(COMMENT beta-reduction)
(VAR
  v_x : term
  m_typ : term
  m_B : term
  m_F : term -> term
)
(RULES
  app( lam(m_typ,\\v_x. m_F v_x), m_B) -> m_F(m_B)
)@.";
    end


let rec split n lst =
  let rec aux n acc lst =
    if n <= 0 then (List.rev acc, lst)
    else match lst with
      | [] -> assert false
      | hd::tl -> aux (n-1) (hd :: acc) tl in
  aux n [] lst

let print_name fmt cst = fprintf fmt "%a_%a" pp_mident (md cst) pp_ident (id cst)

let pp_pattern ar fmt pat =
  let nb = ref 0 in
  let rec aux k fmt = function
  | Var (_,x,n,args) when (n<k) ->
    begin
      List.iter (fun _ -> fprintf fmt "app(") args ;
      fprintf fmt "v_%a" pp_ident x ;
      List.iter (fun pat -> fprintf fmt ",%a)" (aux 0) pat) args
    end
  | Pattern (_,cst,args) ->
    begin
      List.iter (fun _ -> fprintf fmt "app(") args ;
      fprintf fmt "c_%a" print_name cst;
      List.iter (fun pat -> fprintf fmt ",%a)" (aux k) pat) args
    end
  | Var (_,x,n,[]) (* n>=k *) -> fprintf fmt "m_%a" pp_ident x ;
  | Var (_,x,n,a::args) (* n>=k *) ->
    let arity = IdMap.find x ar in
      if arity == 0 then (
        List.iter (fun _ -> fprintf fmt "app(" ) (a::args);
        fprintf fmt "m_%a" pp_ident x;
        List.iter ( fprintf fmt ",%a)" (aux k) ) (a::args)
      ) else (
        let (args1,args2) = split (arity-1) args in
        List.iter (fun _ -> fprintf fmt "app(" ) args2;
        fprintf fmt "m_%a(%a" pp_ident x (aux k) a;
        List.iter ( fprintf fmt ",%a" (aux k) ) args1;
        fprintf fmt ")";
        List.iter ( fprintf fmt ",%a)" (aux k) ) args2
      )
  | Lambda (_,x,p) ->
    fprintf fmt "lam(m_typ,\\v_%a.%a)" pp_ident x (aux (k+1)) p
  | Brackets _ -> ( incr nb; fprintf fmt "b_%i" !nb )
  in
  aux 0 fmt pat

let rec pp_term (ar:int IdMap.t) k fmt term =
  match term with
  | Const (_,cst) -> fprintf fmt "c_%a" print_name cst;
  | Lam (_,x,Some a,b) ->
    fprintf fmt "lam(%a,\\v_%a.%a)" (pp_term ar k) a pp_ident x (pp_term ar (k+1)) b
  | Lam (_,x,None,b) -> failwith "Not implemented: TPDB export for non-annotated abstractions." (*FIXME*)
  | Pi (_,x,a,b) ->
    fprintf fmt "pi(%a,\\v_%a.%a)" (pp_term ar k) a pp_ident x (pp_term ar (k+1)) b
  | DB (_,x,n) when n<k -> fprintf fmt "v_%a" pp_ident x
  | DB (_,x,_) -> fprintf fmt "m_%a" pp_ident x
  | App (DB (_,x,n),a,args) when (n>=k) ->
    let arity = IdMap.find x ar in
    if arity == 0 then (
      List.iter (fun _ -> fprintf fmt "app(" ) (a::args);
      fprintf fmt "m_%a" pp_ident x;
      List.iter ( fprintf fmt ",%a)" (pp_term ar k) ) (a::args)
    ) else (
      let (args1,args2) = split (arity-1) args in
      List.iter (fun _ -> fprintf fmt "app(" ) args2;
      fprintf fmt "m_%a(%a" pp_ident x (pp_term ar k) a;
      List.iter ( fprintf fmt ",%a" (pp_term ar k) ) args1;
      fprintf fmt ")";
      List.iter ( fprintf fmt ",%a)" (pp_term ar k) ) args2
    )
  | App (f,a,args) ->
    begin
      List.iter (fun _ -> fprintf fmt "app(" ) (a::args);
      pp_term ar k fmt f ;
      List.iter ( fprintf fmt ",%a)" (pp_term ar k) ) (a::args)
    end
  | Type _  -> fprintf fmt "type"
  | Kind  -> assert false

let get_bvars r =
  let pat = pattern_of_rule_infos r in
  let rec aux_t k bvars = function
    | Const _ | Kind | Type _ | DB _ -> bvars
    | Lam (_,x,None,b) -> failwith "Not implemented: TPDB export for non-annotated abstractions." (*FIXME*)
    | Lam (_,x,Some a,b) | Pi (_,x,a,b) ->
      let bvars2 = aux_t k bvars a in aux_t (k+1) (x::bvars2) b
    | App (f,a,args) ->
      List.fold_left (aux_t k) bvars (f::a::args)
  in
  let rec aux_p k bvars = function
    | Var (_,_,_,args) | Pattern (_,_,args) ->
      List.fold_left (aux_p k) bvars args
    | Lambda (_,x,p) -> aux_p (k+1) (x::bvars) p
    | Brackets te -> aux_t k bvars te
  in
  let bvars0 = aux_p 0 [] pat in
  aux_t 0 bvars0 r.rhs

let get_arities (p:pattern) : int IdMap.t =
  let rec aux k map = function
    | Var (_,x,n,args) when (n<k) -> List.fold_left (aux k) map args
    | Pattern (_,_,args) -> List.fold_left (aux k) map args
    | Var (_,x,n,args) (* n>=k *) ->
      let map2 = List.fold_left (aux k) map args in
      let ar1 = List.length args in
      let ar = ( try
                   let ar2 = IdMap.find x map2 in
                   if ar2 < ar1 then ar2 else ar1
                 with Not_found -> ar1
               ) in
      IdMap.add x ar map2
    | Lambda (_,x,p) -> aux (k+1) map p
    | Brackets _ -> map
  in
  aux 0 IdMap.empty p

let pp_rule fmt (r:rule_infos) =
  let rec pp_type fmt n =
    fprintf fmt "term";
    if n > 0 then (fprintf fmt " -> "; pp_type fmt (n-1))
  in
  let pat = pattern_of_rule_infos r in
  let arities = get_arities pat in
  (* Variables*)
  fprintf fmt "(VAR\n";
  IdMap.iter (fun x n -> fprintf fmt "  m_%a : %a\n" pp_ident x pp_type n) arities;
  List.iter  (fun x   -> fprintf fmt "  v_%a : term\n" pp_ident x) (get_bvars r) ;
  List.iteri (fun i _ -> fprintf fmt "  b_%i : term\n" (i+1)) (r.constraints) ;
  fprintf fmt ")@.";
  (* Rule *)
  fprintf fmt "(RULES %a -> %a )@.@." (pp_pattern arities) pat (pp_term arities 0) r.rhs

let check () : (unit,confluence_error) error =
  match !file_out with
  | None -> OK ()
  | Some (file,out) ->
    begin
      flush out;
      let cmd = !confluence_command ^ " -p " ^ file in
      debug 1 "Checking confluence : %s" cmd;
      let input = Unix.open_process_in cmd in
      try (
        let answer = input_line input in
        let _ = Unix.close_process_in input in
        if ( String.compare answer "YES" == 0 ) then OK ()
        else (
          do_not_erase_confluence_file := true;
          if ( String.compare answer "NO" == 0 ) then
            Err (NotConfluent cmd)
          else if ( String.compare answer "MAYBE" == 0 ) then
            Err (MaybeConfluent cmd)
          else Err (CCFailure cmd)
        ) )
      with End_of_file -> Err (CCFailure cmd)
    end

let add_constant cst =
  match !file_out with
  | None -> ()
  | Some (file,out) ->
    let fmt = formatter_of_out_channel out in
    fprintf fmt "(FUN c_%a : term)@." pp_name cst

let add_rules lst =
  match !file_out with
  | None -> ()
  | Some (file,out) ->
    let fmt = formatter_of_out_channel out in
    List.iter (pp_rule fmt) lst

let finalize () =
  match !file_out with
  | None -> ()
  | Some (file, fmt) ->
    begin
      close_out fmt;
      ( if !do_not_erase_confluence_file then ()
        else Sys.remove file );

    end
