open Basic
open Format

type preterm =
  | PreType of loc
  | PreId   of loc * string
  | PreQId  of loc * string * string
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * string * preterm option * preterm
  | PrePi   of loc * string option * preterm * preterm

let rec pp_preterm fmt preterm =
  match preterm with
  | PreType _        -> fprintf fmt "Type"
  | PreId (_,v)      -> fprintf fmt "%s" v
  | PreQId (_,md,id)   -> fprintf fmt "%s.%s" md id
  | PreApp (f,a,lst) -> pp_list " " pp_preterm_wp  fmt (f::a::lst)
  | PreLam (_,v,None,b) -> fprintf fmt "%s => %a" v pp_preterm b
  | PreLam (_,v,Some a,b) -> fprintf fmt "%s:%a => %a" v pp_preterm_wp a pp_preterm b
  | PrePi (_,o,a,b)    ->
    ( match o with
      | None   -> fprintf fmt "%a -> %a" pp_preterm_wp a pp_preterm b
      | Some v -> fprintf fmt "%s:%a -> %a" v pp_preterm_wp a pp_preterm b )

and pp_preterm_wp fmt preterm =
  match preterm with
  | PreType _ | PreId _ | PreQId _ as t  -> pp_preterm fmt t
  | t                                    -> fprintf fmt "(%a)" pp_preterm t

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc * string option * string * prepattern list
  | PLambda     of loc * string * prepattern
  | PJoker      of loc


let rec pp_prepattern fmt ppatern =
  let pp_pconst fmt (md,id) =
    match md with
    | None      -> fprintf fmt "%s" id
    | Some md   -> fprintf fmt "%a" Name.pp_ident (Name.make2 md id)
  in
  match ppatern with
  | PPattern (_,md,id,[])       -> pp_pconst fmt (md,id)
  | PPattern (_,md,id,lst)      -> fprintf fmt "%a %a" pp_pconst  (md,id) (pp_list " " pp_prepattern) lst
  | PCondition pte              -> fprintf fmt "{ %a }" pp_preterm pte
  | PJoker _                    -> fprintf fmt "_"
  | PLambda (_,id,p)            -> fprintf fmt "%s => %a" id pp_prepattern p

and pp_prepattern_wp fmt = function
  | PLambda (_,_,_)
  | PPattern (_,_,_,_::_) as p  -> fprintf fmt "(%a)" pp_prepattern p
  | p                           -> pp_prepattern fmt p

type pdecl      = loc * string

let pp_pdecl fmt (_,id) = Format.fprintf fmt "%s" id

type pcontext   = pdecl list

let pp_pcontext fmt ctx =
  pp_list ".\n" (fun out (_,x) ->
      fprintf fmt "%s" x) fmt (List.rev ctx)

type prule      = loc * (string option * string) option * pdecl list * string option * string * prepattern list * preterm

(* TODO : implements this *)
let pp_prule fmt ((_, pname, pdecl, pid, id, prepatterns, prete):prule) : unit  =
  let name = match pname with | None -> "" | Some qid ->
    let prefix = match fst qid with | None -> "" | Some md -> md^"." in
    "{"^prefix^id^"}"
  in
  match pid with
  | Some m ->
    let cst = Name.make2 m id in
    fprintf fmt "[%a] %a %a --> %a %s" (pp_list "," pp_pdecl) pdecl Name.pp_ident cst
      (pp_list " " pp_prepattern) prepatterns pp_preterm prete name
  | None ->
    fprintf fmt "[%a] %s %a --> %a %s" (pp_list "," pp_pdecl) pdecl id
      (pp_list " " pp_prepattern) prepatterns pp_preterm prete name
