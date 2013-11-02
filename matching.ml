
open Types

(* Compilation of rewrite rules to decision tree as in:
 * Compiling Pattern Matching to Good Decision Trees (Maranget, 2008)
 * *)

module H = Hashtbl.Make(
struct 
  type t        = ident*ident
  let hash      = Hashtbl.hash 
  let equal (m,v) (m',v') = 
    ident_eq v v' && ident_eq m m'
end ) 

(* *** Types *** *)

type line = { pats:pattern array ; right:term ; env_size:int ; }
type pMat = line list 
type union = PMat of pMat | Term of term
type partition = { cases:( (ident*ident) * union ) list ; default: pMat option ; }

(* *** Debug *** *)
(*
let dump_gdt (id:string) (g:gdt) = 
  let rec aux = function
    | Leaf te                     -> Global.eprint ("Leaf : "^Error.string_of_term te^"\n")
    | Switch (c,cases,def)        ->
        begin
          Global.eprint ("Switch ( "^string_of_int c ^") [\n");
          List.iter (fun ((m,v),g) -> Global.eprint ("Case "^m^"."^v^": ") ; aux g ) cases ;
          (match def with
             | None       -> ()
             | Some g     -> (Global.eprint "Def: " ; aux g) ) ;
          Global.eprint ("]\n")
        end
  in
    Global.eprint (" --------> GDT FOR "^id^"\n");
    aux g;
    Global.eprint " <-------- \n"

let dump_line (id:string) (l:line) =
    Global.eprint ( " [ " ^ string_of_int l.env_size ^ " ] " ^ id ^ " " ) ;
    Array.iter (fun p -> Global.eprint (Error.string_of_pattern p^"\t")) l.pats ;
    Global.eprint ( " --> " ^ Error.string_of_term l.right ^ "\n" )

let dump_pMat (id:string) (pm:pMat) =
  Global.eprint (" --------> PMAT FOR "^id^"\n");
  List.iter (dump_line id) pm ;
  Global.eprint " <-------- \n"
 *)
(* *** Internal functions *** *)

let pMat_from_rules (rs:rule list) : pMat =  
  match rs with
    | []                        -> assert false
    | (ctx,((_,v),pats),ri)::rs'  ->
        let arity = Array.length pats in
          { pats=pats ; right=ri ; env_size=List.length ctx } :: (
            List.map (
              fun (ctx',((lc,v'),pats'),ri') ->
                if Array.length pats' != arity then 
                  raise ( PatternError ( lc , "All the rules must have the same arity." ) ) 
                else if (not (ident_eq v v')) then 
                  raise ( PatternError ( lc , "All the rules must have the same head symbol." ) )  
                else { pats=pats' ; right=ri' ; env_size=List.length ctx' }
            ) rs'
          ) 

let union_from_lines (lst:line list) : union = 
  match lst with
    | []        -> assert false
    | x::lst'   -> 
        let arity = Array.length x.pats in
          if arity = 0 then Term x.right
          else 
            begin
              List.iter (
                fun y -> if Array.length y.pats != arity then 
                  raise ( PatternError ( dloc , "All the rules must have the same arity." ) ) 
              ) lst';
              PMat lst
            end 

let specialize (c:int) (l:line) (args:pattern array) : line = 
  let n = Array.length l.pats - 1 in
    { l with pats = 
        Array.init (n + Array.length args) (
          fun i ->
            if i < c      then l.pats.(i)       (* [ 0 - c [ *)
            else if i < n then l.pats.(i+1)     (* [ c - n [ *)
            else               args.(i-n)       (* [ n - n+nargs [ *)
        )
    }

let partition (pm:pMat) (c:int) : partition = 
  let hs  = H.create 47         in 
  let add id l = 
    ( try H.replace hs id (l::(H.find hs id))
      with Not_found -> H.add hs id [l] ) in
  let def = ref [] in
    List.iter (
      fun z ->
        match z.pats.(c) with
          | Pattern ((_,m,v),args)      -> add (m,v) (specialize c z args)
          | Dash _ | Var _              -> def := z::(!def)
    ) pm ;
    let cases = H.fold (
      fun id lines l ->
        (id,union_from_lines lines)::l
    ) hs [] in
      match !def with
        | []    -> { cases=cases ; default=None ; }
        | _     -> { cases=cases ; default=Some !def ; }

let getColumn (l:pattern array) : int option =
  let rec aux i = 
    if i < Array.length l then
      ( match l.(i) with
        | Pattern _     -> Some i
        | _             -> aux (i+1) )
    else None
  in aux 0

let rec reorder (ord:int array) (k:int) : term -> term = function
  | App args                    -> mk_uapp (List.map (reorder ord k) args)
  | Lam (l,x,a,f)               -> mk_lam l x (reorder ord k a) (reorder ord (k+1) f)
  | Pi  (l,x,a,b)               -> mk_pi  l x (reorder ord k a) (reorder ord (k+1) b)
  | DB (l,x,n) when (n>=k)      -> 
      begin 
        (*assert (n-k < Array.length ord);*) 
        let n_db = ord.(n-k) in
          if n_db = (-1) then raise ( PatternError ( dloc , "Free variables on the right-hand side of a rule should also appear in the left-hand side." ) ) 
          else mk_db l x (n_db + k) 
      end
  | t                   -> t 

let get_term (l:line) : term = 
  (*dump_line "???" l ; *)
  let ord = Array.make l.env_size (-1) in 
    Array.iteri (
      fun i p ->
        match p with
          | Var (_,_,n)   -> 
              if ord.(n) = (-1) then ord.(n) <- i
              else raise ( PatternError ( dloc , "Non linear rule detected." ) ) 
          | _       -> ()
    ) l.pats ;
    reorder ord 0 l.right

let rec cc (pm:pMat) : gdt =
  let first = match pm with | [] -> assert false | x::_ -> x in
    match getColumn first.pats with
      (* La 1ere ligne ne contient que des Var *)
      | None      -> Leaf ( get_term first )
      (* Colonne c contient un pattern *)
      | Some c    ->
          let par = partition pm c in
          let cases = 
            List.rev_map ( 
              function 
                | ( id , Term te  )     -> ( id , Leaf te ) (*FIXME reorder ???*)
                | ( id , PMat pm' )     -> ( id , cc pm' )
            ) par.cases in
            ( match par.default with
                | None           -> Switch (c,cases,None)
                | Some pm'       -> Switch (c,cases,Some (cc pm'))
            ) 

let safe_find m v cases =
  try Some ( snd ( List.find (fun ((m',v'),_) -> ident_eq v v' && ident_eq m m') cases ) )
  with Not_found -> None

let rec replace m v g = function
  | []                          -> assert false
  | (((m',v'),g') as c)::lst    ->
      if ident_eq v v' && ident_eq m m' then ((m,v),g)::lst
      else c::(replace m v g lst)

let rec add_lines pm = function (*FIXME to be tested*)
    | Leaf te as g              -> g 
    | Switch (i,cases,def)      -> 
        begin
          let p = partition pm i in
          let cases2 = List.fold_left (
            fun ca ((m,v),u) -> 
              match safe_find m v cases , u with
                | None , Term te        -> cases@[((m,v),Leaf te)] (*FIXME reorder ???*)
                | None , PMat pm'       -> cases@[((m,v),cc pm')]
                | Some g , Term te      -> assert false (*FIXME*)
                | Some g , PMat pm'     -> replace m v (add_lines pm' g) cases
          ) cases p.cases in
          let def2 = match ( def , p.default ) with
            | _ , None          -> def
            | None , Some pm'   -> Some (cc pm')
            | Some d , Some pm' -> Some (add_lines pm' d)
          in
            Switch (i,cases2,def2)
        end

let add_rw (n,g) rs = 
  match pMat_from_rules rs with
    | []                -> assert false
    | l::_ as pm        ->
        if Array.length l.pats != n then 
          raise ( PatternError ( dloc , "Arity mismatch: all the rules must have the same arity." ) ) 
        else
          ( n , add_lines pm g )

let get_rw (rs:rule list) : int*gdt =
  let pm  = pMat_from_rules rs in
    (* dump_pMat "???" pm ; *)
  let gdt = cc pm in
     (*dump_gdt "???" gdt ; *)
  let le = match pm with | x::_ -> Array.length x.pats | _ -> assert false in
    ( le , gdt )
