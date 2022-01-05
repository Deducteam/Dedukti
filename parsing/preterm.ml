open Kernel.Basic
open Format

exception AppliedGuardedTerm of loc
exception BetaRedexInLHS of loc

type preterm =
  | PreType of loc
  | PreId of loc * ident
  | PreQId of loc * name
  | PreApp of preterm * preterm * preterm list
  | PreLam of loc * ident * preterm option * preterm
  | PrePi of loc * ident option * preterm * preterm

let rec pp_preterm fmt preterm =
  match preterm with
  | PreType _                -> fprintf fmt "Type"
  | PreId (_, v)             -> pp_ident fmt v
  | PreQId (_, cst)          -> fprintf fmt "%a" pp_name cst
  | PreApp (f, a, lst)       -> pp_list " " pp_preterm_wp fmt (f :: a :: lst)
  | PreLam (_, v, None, b)   -> fprintf fmt "%a => %a" pp_ident v pp_preterm b
  | PreLam (_, v, Some a, b) ->
      fprintf fmt "%a:%a => %a" pp_ident v pp_preterm_wp a pp_preterm b
  | PrePi (_, o, a, b)       -> (
      match o with
      | None   -> fprintf fmt "%a -> %a" pp_preterm_wp a pp_preterm b
      | Some v ->
          fprintf fmt "%a:%a -> %a" pp_ident v pp_preterm_wp a pp_preterm b)

and pp_preterm_wp fmt preterm =
  match preterm with
  | (PreType _ | PreId _ | PreQId _) as t -> pp_preterm fmt t
  | t -> fprintf fmt "(%a)" pp_preterm t

let rec loc_of_preterm (pte : preterm) : loc =
  match pte with
  | PreType l -> l
  | PreId (l, _) -> l
  | PreQId (l, _) -> l
  | PreApp (t, _, _) -> loc_of_preterm t
  | PreLam (l, _, _, _) -> l
  | PrePi (l, _, _, _) -> l

type prepattern =
  | PCondition of preterm
  | PPattern of loc * mident option * ident * prepattern list
  | PLambda of loc * ident * prepattern
  | PJoker of loc * prepattern list
  | PApp of prepattern list

let rec pp_prepattern fmt ppatern =
  let pp_pconst fmt (md, id) =
    match md with
    | None    -> pp_ident fmt id
    | Some md -> fprintf fmt "%a" pp_name (mk_name md id)
  in
  match ppatern with
  | PPattern (_, md, id, [])  -> pp_pconst fmt (md, id)
  | PPattern (_, md, id, lst) ->
      fprintf fmt "%a %a" pp_pconst (md, id) (pp_list " " pp_prepattern) lst
  | PCondition pte            -> fprintf fmt "{ %a }" pp_preterm pte
  | PJoker _                  -> fprintf fmt "_"
  | PLambda (_, id, p)        -> fprintf fmt "%a => %a" pp_ident id
                                   pp_prepattern p
  | PApp plist                -> fprintf fmt "(%a)"
                                   (pp_list " " pp_prepattern) plist

let rec loc_of_prepat (ppat : prepattern) : loc =
match ppat with
  | PPattern (l, _, _, _) -> l
  | PCondition pte        ->
      let (l,c) = of_loc (loc_of_preterm pte) in
      mk_loc l (c-1)
  | PJoker (l, _)         -> l
  | PLambda (l, _, _)     -> l
  | PApp []               -> assert false
  | PApp (h::_)          ->
      let (l,c) = of_loc (loc_of_prepat h) in
      mk_loc l (c-1)

let rec clean_pre_pattern (ppat : prepattern) : prepattern =
  match ppat with
  (* The grammar ensures that the list cannot be empty. *)
  | PApp [] -> assert false
  | PApp (h::tl) ->
      begin
        match clean_pre_pattern h with
        | PPattern (l,md,id,args) ->
            PPattern(l,md,id,List.map clean_pre_pattern (args @ tl))
        | PCondition pte ->
            if tl = []
            then PCondition pte
            else
              raise (AppliedGuardedTerm (loc_of_prepat ppat))
        | PJoker (l, args) ->
            PJoker(l, List.map clean_pre_pattern (args @ tl))
        | PLambda (l, id, p) ->
            if tl = []
            then PLambda (l, id, clean_pre_pattern p)
            else
              raise (BetaRedexInLHS (loc_of_prepat ppat))
        (* clean_pre_pattern suppresses all PApp*)
        | PApp _ -> assert false
      end
  | PPattern (l,md,id,args) ->
      PPattern(l,md,id,List.map clean_pre_pattern args)
  | PCondition pte -> PCondition pte
  | PJoker (l, args) ->
      PJoker(l, List.map clean_pre_pattern args)
  | PLambda (l, id, p) ->
      PLambda (l, id, clean_pre_pattern p)

(* and pp_prepattern_wp fmt = function
 *   | PLambda (_,_,_)
 *   | PPattern (_,_,_,_::_) as p  -> fprintf fmt "(%a)" pp_prepattern p
 *   | p                           -> pp_prepattern fmt p *)

type pdecl = (loc * ident) * preterm option

let pp_pdecl fmt = function
  | (_, id), None    -> pp_ident fmt id
  | (_, id), Some ty -> fprintf fmt "%a : %a" pp_ident id pp_preterm ty

type pcontext = pdecl list

let pp_pcontext fmt ctx = pp_list ".\n" pp_pdecl fmt (List.rev ctx)

type prule =
  loc
  * (mident option * ident) option
  * pdecl list
  * mident option
  * ident
  * prepattern list
  * preterm

(* TODO : implements this *)
let pp_prule fmt ((_, pname, pdecl, pid, id, prepatterns, prete) : prule) : unit
    =
  let name =
    match pname with
    | None     -> ""
    | Some qid ->
        let prefix =
          match fst qid with None -> "" | Some md -> string_of_mident md ^ "."
        in
        "{" ^ prefix ^ string_of_ident id ^ "}"
  in
  match pid with
  | Some m ->
      let cst = mk_name m id in
      fprintf fmt "[%a]\n  %a %a\n-->\n  %a %s" (pp_list ", " pp_pdecl) pdecl
        pp_name cst
        (pp_list " " pp_prepattern)
        prepatterns pp_preterm prete name
  | None   ->
      fprintf fmt "[%a]\n  %a %a\n-->\n  %a %s" (pp_list ", " pp_pdecl) pdecl
        pp_ident id
        (pp_list " " pp_prepattern)
        prepatterns pp_preterm prete name
