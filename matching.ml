
open Types

(* Compilation of rewrite rules to decision tree as in:
 * Compiling Pattern Matching to Good Decision Trees (Maranget, 2008)
 * *)


(* *** Types *** *)

type line = { pats:pattern array ; right:term ; env_size:int ; }
type pMat = line list 
type union = PMat of pMat | Term of term
type partition = { cases:( (string*string) * union ) list ; default: pMat option ; }

(* *** Debug *** *)

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

(* *** Internal functions *** *)

let pMat_from_rules (rs:rule list) : pMat = 
  match rs with
    | []                        -> assert false
    | (k,((_,m,v),pats),ri)::rs'  ->
        let arity = Array.length pats in
          { pats=pats ; right=ri ; env_size=k } :: (
            List.map (
              fun (k',((lc,m',v'),pats'),ri') ->
                if Array.length pats' != arity then raise ( PatternError ( lc , "Arity mismatch: all the rules must have the same arity." ) ) 
                else if m' != m || v != v'     then raise ( PatternError ( lc , "Head symbol mismatch: all the rules must have the same head symbol." ) ) 
                else { pats=pats' ; right=ri' ; env_size=k' }
            ) rs'
          )

let union_from_lines (lst:line list) : union = 
  match lst with
    | []        -> assert false
    | x::lst'   -> 
        let arity = Array.length x.pats in
          if arity = 0 then Term x.right
          else (
                List.iter (
                  fun y -> if Array.length y.pats != arity then raise ( PatternError ( dloc , "Arity mismatch: all the rules must have the same arity." ) ) 
                ) lst';
                PMat lst
          ) 

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
  let hs : (string*string,line list) Hashtbl.t  = Hashtbl.create 47 in
  let add id l = 
    ( try Hashtbl.replace hs id (l::(Hashtbl.find hs id))
      with Not_found -> Hashtbl.add hs id [l] ) in
  let def = ref [] in
    List.iter (
      fun z ->
        match z.pats.(c) with
          | Pattern ((_,m,v),args)      -> add (m,v) (specialize c z args)
          | Dash _ | Var _              -> def := z::(!def)
    ) pm ;
    let cases = Hashtbl.fold (
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
  | DB n when n<k       -> DB n
  | App args            -> App (List.map (reorder ord k) args)
  | Lam (a,f)           -> Lam (reorder ord k a, reorder ord (k+1) f)
  | Pi  (a,b)           -> Pi  (reorder ord k a, reorder ord (k+1) b)
  | DB n (* n>=k *)     -> 
      begin 
    (*assert (n-k < Array.length ord);*) 
        let n_db = ord.(n-k) in
          if n_db = (-1) then raise ( PatternError ( dloc , "Free variables on the right-hand side of a rule should also appear in the left-hand side." ) ) 
          else DB (n_db + k) 
      end
  | t                   -> t 

let get_term (l:line) : term = 
  (*dump_line "???" l ; *)
  let ord = Array.make l.env_size (-1) in 
    Array.iteri (
      fun i p ->
        match p with
          | Var n   -> 
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
                | ( id , Term te  )     -> ( id , Leaf te )
                | ( id , PMat pm' )     -> ( id , cc pm' )
            ) par.cases in
            ( match par.default with
                | None           -> Switch (c,cases,None)
                | Some pm'       -> Switch (c,cases,Some (cc pm'))
            ) 

(* *** Interface *** *)

let get_rw (rs:rule list) : int*gdt =
  let pm  = pMat_from_rules rs in
    (* dump_pMat "???" pm ; *)
  let gdt = cc pm in
     (*dump_gdt "???" gdt ; *)
  let le = match pm with | x::_ -> Array.length x.pats | _ -> assert false in
    ( le , gdt )
