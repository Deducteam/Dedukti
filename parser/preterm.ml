open Basic
open Format

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm option * preterm
  | PrePi   of loc * ident option * preterm * preterm

let rec pp_preterm fmt preterm =
  match preterm with
  | PreType _        -> fprintf fmt "Type"
  | PreId (_,v)      -> pp_ident fmt v
  | PreQId (_,m,v)   -> fprintf fmt "%a.%a" pp_ident m pp_ident v
  | PreApp (f,a,lst) -> pp_list " " pp_preterm_wp  fmt (f::a::lst)
  | PreLam (_,v,None,b) -> fprintf fmt "%a => %a" pp_ident v pp_preterm b
  | PreLam (_,v,Some a,b) -> fprintf fmt "%a:%a => %a" pp_ident v pp_preterm_wp a pp_preterm b
  | PrePi (_,o,a,b)    ->
    ( match o with
      | None   -> fprintf fmt "%a -> %a" pp_preterm_wp a pp_preterm b
      | Some v -> fprintf fmt "%a:%a -> %a" pp_ident v pp_preterm_wp a pp_preterm b )

and pp_preterm_wp fmt preterm =
  match preterm with
  | PreType _ | PreId _ | PreQId _ as t  -> pp_preterm fmt t
  | t                                    -> fprintf fmt "(%a)" pp_preterm t

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc * ident option * ident * prepattern list
  | PLambda     of loc * ident * prepattern
  | PJoker      of loc


let rec pp_prepattern fmt ppatern =
  let pp_pconst fmt mid =
    match mid with
    | ( None , id )     -> pp_ident fmt id
    | ( Some md , id )  -> fprintf fmt "%a.%a" pp_ident md pp_ident id
  in
  match ppatern with
  | PPattern (_,md,id,[])       -> pp_pconst fmt (md,id)
  | PPattern (_,md,id,lst)      -> fprintf fmt "%a %a" pp_pconst (md,id) (pp_list " " pp_prepattern) lst
  | PCondition pte              -> fprintf fmt "{ %a }" pp_preterm pte
  | PJoker _                    -> fprintf fmt "_"
  | PLambda (_,id,p)            -> fprintf fmt "%a => %a" pp_ident id pp_prepattern p

and pp_prepattern_wp fmt = function
  | PLambda (_,_,_)
  | PPattern (_,_,_,_::_) as p  -> fprintf fmt "(%a)" pp_prepattern p
  | p                           -> pp_prepattern fmt p

type pdecl      = loc * ident

let pp_pdecl fmt (_,id) = pp_ident fmt id

type pcontext   = pdecl list

let pp_pcontext fmt ctx =
  pp_list ".\n" (fun out (_,x) ->
      fprintf fmt "%a" pp_ident x) fmt (List.rev ctx)

type prule      = loc * (ident option * ident) option * pdecl list * ident option * ident * prepattern list * preterm

let pp_prule fmt ((_, pname, pdecl, pid, id, prepatterns, prete):prule) : unit  =
  let name = match pname with | None -> "" | Some qid ->
    let prefix = match fst qid with | None -> "" | Some md -> (string_of_ident md)^"." in
    "{"^prefix^string_of_ident id^"}"
  in
  match pid with
  | Some m ->
    fprintf fmt "[%a] %a.%a %a --> %a %s" (pp_list "," pp_pdecl) pdecl pp_ident m pp_ident id
      (pp_list " " pp_prepattern) prepatterns pp_preterm prete name
  | None ->
    fprintf fmt "[%a] %a %a --> %a %s" (pp_list "," pp_pdecl) pdecl pp_ident id
      (pp_list " " pp_prepattern) prepatterns pp_preterm prete name

type pbox_term = loc * pcontext * preterm

type pmtype =
  | PImpl of loc * pmtype * pmtype
  | PForall of loc * ident * pbox_term * pmtype
  | PBoxTy of pbox_term

type pmterm =
  | PMLamF of loc * ident * pbox_term * pmterm
  | PMLamI of loc * ident * pmterm * pmterm
  | Case of loc * ident * pcase list
  | PBoxTe of pbox_term

and pcase = pbox_term * pmterm
