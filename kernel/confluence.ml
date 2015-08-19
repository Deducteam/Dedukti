open Basics
open Printf
open Term
open Rule

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
      Basics.debug "Confluence temporary file:%s" file;
      file_out := (Some (file,out));
      fprintf out "\
(FUN
  lam : term -> (term -> term) -> term
  app : term -> term -> term
  pi  : term -> (term -> term) -> term
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
)\n"
    end

let rec split n lst =
  if n <= 0 then ( [], lst )
  else
    match lst with
    | [] -> assert false
    | hd::tl ->
      let (x,y) = split (n-1) tl in
      ( hd::x, tl )

let pp_pattern ar out pat =
  let nb = ref 0 in
  let rec aux k out = function
  | Var (_,x,n,args) when (n<k) ->
    begin
      List.iter (fun _ -> fprintf out "app(") args ;
      fprintf out "v_%a" pp_ident x ;
      List.iter (fun pat -> fprintf out ",%a)" (aux 0) pat) args
    end
  | Pattern (_,m,v,args) ->
    begin
      List.iter (fun _ -> fprintf out "app(") args ;
      fprintf out "c_%a_%a" pp_ident m pp_ident v ;
      List.iter (fun pat -> fprintf out ",%a)" (aux k) pat) args
    end
  | Var (_,x,n,[]) (* n>=k *) -> fprintf out "m_%a" pp_ident x ;
  | Var (_,x,n,a::args) (* n>=k *) ->
    let arity = IdMap.find x ar in
      if arity == 0 then (
        List.iter (fun _ -> fprintf out "app(" ) (a::args);
        fprintf out "m_%a" pp_ident x;
        List.iter ( fprintf out ",%a)" (aux k) ) (a::args)
      ) else (
        let (args1,args2) = split (arity-1) args in
        List.iter (fun _ -> fprintf out "app(" ) args2;
        fprintf out "m_%a(%a" pp_ident x (aux k) a;
        List.iter ( fprintf out ",%a" (aux k) ) args1;
        fprintf out ")";
        List.iter ( fprintf out ",%a)" (aux k) ) args2
      )
  | Lambda (_,x,p) ->
    fprintf out "lam(m_typ,\\v_%a.%a)" pp_ident x (aux (k+1)) p
  | Brackets _ -> ( incr nb; fprintf out "b_%i" !nb )
  in
  aux 0 out pat

let rec pp_term (ar:int IdMap.t) k out = function
  | Const (_,m,v) -> fprintf out "c_%a_%a" pp_ident m pp_ident v
  | Lam (_,x,Some a,b) ->
    fprintf out "lam(%a,\\v_%a.%a)" (pp_term ar k) a pp_ident x (pp_term ar (k+1)) b
  | Lam (_,x,None,b) -> assert false (*FIXME*)
  | Pi (_,x,a,b) ->
    fprintf out "pi(%a,\\v_%a.%a)" (pp_term ar k) a pp_ident x (pp_term ar (k+1)) b
  | DB (_,x,n) when n<k -> fprintf out "v_%a" pp_ident x
  | DB (_,x,_) -> fprintf out "m_%a" pp_ident x
  | App (DB (_,x,n),a,args) when (n>=k) ->
    let arity = IdMap.find x ar in
    if arity == 0 then (
      List.iter (fun _ -> fprintf out "app(" ) (a::args);
      fprintf out "m_%a" pp_ident x;
      List.iter ( fprintf out ",%a)" (pp_term ar k) ) (a::args)
    ) else (
      let (args1,args2) = split (arity-1) args in
      List.iter (fun _ -> fprintf out "app(" ) args2;
      fprintf out "m_%a(%a" pp_ident x (pp_term ar k) a;
      List.iter ( fprintf out ",%a" (pp_term ar k) ) args1;
      fprintf out ")";
      List.iter ( fprintf out ",%a)" (pp_term ar k) ) args2
    )
  | App (f,a,args) ->
    begin
      List.iter (fun _ -> fprintf out "app(" ) (a::args);
      pp_term ar k out f ;
      List.iter ( fprintf out ",%a)" (pp_term ar k) ) (a::args)
    end
  | Kind | Type _  -> assert false

let get_bvars r =
  let pat = (Pattern (r.l,r.md,r.id,r.args)) in
  let rec aux_t k bvars = function
    | Const _ | Kind | Type _ | DB _ -> bvars
    | Lam (_,x,None,b) -> assert false (*FIXME*)
    | Lam (_,x,Some a,b) | Pi (_,x,a,b) ->
      let bvars2 = aux_t k bvars a in aux_t (k+1) (x::bvars2) b
    | App (f,a,args) ->
      List.fold_left (aux_t k) bvars (f::a::args)
  in
  let rec aux_p k bvars = function
    | Var (_,_,_,args) | Pattern (_,_,_,args) ->
      List.fold_left (aux_p k) bvars args
    | Lambda (_,x,p) -> aux_p (k+1) (x::bvars) p
    | Brackets te -> aux_t k bvars te
  in
  let bvars0 = aux_p 0 [] pat in
  aux_t 0 bvars0 r.rhs

let get_arities (ctx:context) (p:pattern) : int IdMap.t =
  let rec aux k map = function
    | Var (_,x,n,args) when (n<k) -> List.fold_left (aux k) map args
    | Pattern (_,m,v,args) -> List.fold_left (aux k) map args
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

let pp_rule out (r:rule_infos) =
  let pp_type out n =
    for i=1 to n do fprintf out "term -> " done;
    fprintf out "term"
  in
  let pat = (Pattern (r.l,r.md,r.id,r.args)) in
  let arities = get_arities r.ctx pat in
  (* Variables*)
  fprintf out "(VAR\n";
  IdMap.iter (fun x n -> fprintf out "  m_%a : %a\n" pp_ident x pp_type n) arities;
  List.iter  (fun x -> fprintf out "  v_%a : term\n" pp_ident x) (get_bvars r) ;
  List.iteri (fun i _ -> fprintf out "  b_%i : term\n" (i+1)) (r.constraints) ;
  fprintf out ")\n";
  (* Rule *)
  fprintf out "(RULES %a -> %a )\n\n" (pp_pattern arities) pat (pp_term arities 0) r.rhs

let check () : (unit,confluence_error) error =
  match !file_out with
  | None -> OK ()
  | Some (file,out) ->
      begin
        let cmd = !confluence_command ^ " -p " ^ file in
        flush out;
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

let add_constant md id =
  match !file_out with
  | None -> ()
  | Some (file,out) ->
    fprintf out "(FUN c_%a_%a : term)\n" pp_ident md pp_ident id

let add_rules lst =
  match !file_out with
  | None -> ()
  | Some (file,out) -> List.iter (pp_rule out) lst

let finalize () =
  match !file_out with
  | None -> ()
  | Some (file,out) ->
    begin
      close_out out;
      ( if !do_not_erase_confluence_file then ()
        else Sys.remove file );

    end
