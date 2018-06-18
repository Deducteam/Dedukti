open Basic
open Term
open Rule
open Sizematrix
open Callgraph

(** A table linking each symbol with the list of symbols which must be strictly after *)
let must_be_str_after : (name, (name * name) list) Hashtbl.t  =
  Hashtbl.create 5
(* Here 5 is perfectly arbitrary *)

(** A table linking each symbol with the list of symbols which effectively are after (after is a pre-order, it is possible that [a] is after [b] and [b] is after [a]) *)
let after : (name, name list) Hashtbl.t =
  Hashtbl.create 5
(* Here again 5 is arbitrary *)


(** Adding the element a at the begining of the list accessed in ht with key id *)
let updateHT : ('a,'b list) Hashtbl.t -> 'a -> 'b -> unit =
  fun ht id x ->
    let add_in_list x l=
      if List.mem x l
      then l
      else x::l
    in
    if Hashtbl.mem ht id
    then
      Hashtbl.replace ht id (add_in_list x (Hashtbl.find ht id))
    else
      Hashtbl.add ht id [x]


(** The tarjan algorith to compute the strongly connected component of a graph *)
let tarjan : (name, name list) Hashtbl.t -> name list list = fun graph ->
  let num=ref 0 and p = ref [] and partition = ref []
  and numHT= Hashtbl.create 5 and numAcc = Hashtbl.create 5 in
  let rec parcours v=
    Hashtbl.add numHT v !num;
    Hashtbl.add numAcc v !num;
    incr num;
    p := v::!p;
    let parcours_intern w=
      if not (Hashtbl.mem numHT w)
      then
        (parcours w;
         Hashtbl.replace numAcc v (min (Hashtbl.find numAcc v) (Hashtbl.find numAcc w)))
      else
        (if List.mem w !p
         then Hashtbl.replace numAcc v (min (Hashtbl.find numAcc v) (Hashtbl.find numHT w)))
    in
    List.iter parcours_intern (Hashtbl.find graph v);
    if (Hashtbl.find numAcc v)=(Hashtbl.find numHT v)
    then
      begin
	let default_name= mk_name (mk_mident "") (mk_ident "") in
        let c=ref [] and w=ref default_name in
        while not (name_eq !w v)
        do
          w:= List.hd !p;
          p:= List.tl !p;
          c:= !w::!c;
        done;
        partition:=!c::!partition
      end
  in
  Hashtbl.iter
    (fun v -> fun _ -> if not (Hashtbl.mem numHT v) then parcours v)
    graph;
  !partition


type position =  Global | Argument | Negative

let under : position -> position =
  function
  | Global -> Argument
  | _ -> Negative

let rec right_most : term -> term = function
  | Kind                 -> assert false
  | Pi(_,_,_,a)          -> right_most a
  | App(Lam(_),_,_) as t -> t
  | App(a,_,_)           -> right_most a
  | Lam(_,_,_,a)         -> assert false
  | DB(_)                -> assert false
  | t                    -> t


let are_equiv : name -> name -> name list list -> bool = fun a b l ->
  List.exists (fun x -> (List.mem a x) && (List.mem b x)) l

let pp_HT pp_key pp_elt fmt ht=
  Hashtbl.iter (fun a b -> Format.fprintf fmt " - %a : %a@." pp_key a pp_elt b) ht
    
      
let rec constructors_infos : position -> name -> term -> term -> unit =
  fun posit f typ rm ->
    match rm with
    | Type _ ->
      begin
        if not (Hashtbl.mem after f)
        then (Hashtbl.add must_be_str_after f []; Hashtbl.add after f [])
      end
    | _ -> ();
    match typ with
    | Kind -> assert false
    | DB(_,_,_) | Type _ -> ()
    | App(a,_,_) | Lam(_,_,_,a) -> constructors_infos posit f a rm
    | Pi(_,_,lhs,rhs) ->
       begin
	 constructors_infos posit f rhs rm;
	 constructors_infos (under posit) f lhs rm
       end
    | Const(_,g) ->
      begin
        match find_stat g  with
        | Set_constructor ->
          begin
            match rm with
            | Type _ ->
              updateHT after f g; updateHT must_be_str_after f (g,f)
            | Const(_,g2) ->
              begin
                updateHT after g2 g;
                match posit with
                | Negative -> updateHT must_be_str_after g2 (g,f)
                | _ -> ()
              end
            (* TODO : Understand when this case can occur *)
	    | _ -> assert false
	  end
        | _ -> if posit= Negative then update_result f NonPositive 
      end



let str_positive :
  name list list -> (name, (name * name) list) Hashtbl.t -> unit =
  fun l ht ->
    Hashtbl.iter (
      fun a b ->
        List.iter (
          fun (x,y) ->
            if (are_equiv a x l)
            then update_result y NonPositive) b
    ) ht
	

let rec name_var : name -> int -> int -> pattern -> pattern =
  fun f i n ->
    function
    | Var(loc, _, k, ll) when k>=n      ->
      Var(
        loc,
        mk_ident ("x"^(string_of_int i)^"_"^(string_of_int (k-n))),
        k,
        List.map (name_var f i n) ll
      )
    | Var(loc, _, k, ll)                ->
      Var(loc, mk_ident "bound", k, List.map (name_var f i n) ll)
    | Pattern(loc, g, ll)               ->
      Pattern(loc, g, List.map (name_var f i n) ll)
    | Lambda(loc, _, pat)               ->
      Lambda(loc, mk_ident "bound", name_var f i (n+1) pat)
    | Brackets(DB(loc, _, k)) when k>=n ->
      Var(
        loc,
        mk_ident ("x"^(string_of_int i)^"_"^(string_of_int (k-n))),
        k,
        []
      )
    | Brackets(DB(loc, _, k))           ->
      Var(loc, mk_ident "bound", k, [])
    | Brackets(_)                       ->
      update_result f UsingBrackets; raise Exit

let rec occurs : ident -> pattern -> bool =
  fun nk ->
    function
    | Var(_, nn, _, _ ) when nk=nn -> true
    | Var(_, _ , _, ll)            -> List.exists (occurs nk) ll
    | Pattern(_, _, ll)            -> List.exists (occurs nk) ll
    | Lambda(_, _, pat)            -> occurs nk pat
    | Brackets(_)                  -> assert false
      
let rec zip_with_int : 'a list -> 'b list -> int -> ('a * 'b * int) list=
  fun l1 l2 n->
    match l1,l2 with
    | []   , []    -> []
    | a::t1, b::t2 -> (a,b,n)::(zip_with_int t1 t2 n)
    | _            -> assert false
       
let rec solve : name -> (pattern * pattern * int) list -> bool =
  fun f l ->
    match l with
    | []                                                                ->
      true
    | (Var(_, n1, _, l1), Var(_, n2, _, l2), n)::tl
      when (n1=n2 && l1=[] && l2=[]) ->
      solve f tl
    | (Var(_, nk, k, ll)  , t2             , n)::tl when (k>=n && ll=[]) ->
      elim f nk t2 tl n
    | (t1             , Var(_, nk, k, ll)  , n)::tl when (k>=n && ll=[]) ->
      elim f nk t1 tl n
    | (Pattern(_, f, l1), Pattern(_, g, l2), n)::tl when f=g             ->
      solve f ((zip_with_int l1 l2 n)@tl)
    | (Lambda(_, _, t1) , Lambda(_, _, t2) , n)::tl                      ->
      solve f ((t1, t2, n+1)::tl)
    | (Var(loc, _, k, ll), t2             , n)::tl when ll!= []          ->
      update_result f NotHandledRewritingTypeLevel; true
    | (t1             , Var(loc, _, k, ll), n)::tl when ll!= []          ->
      update_result f NotHandledRewritingTypeLevel; true
    | _ -> false
and elim : name -> ident -> pattern -> (pattern * pattern * int) list ->
  int -> bool
  =fun f nk t l n ->
    let rec lift : ident -> pattern -> pattern -> pattern
      = fun nk t ->
        function
        | Var(_,nn,_,_)               when nk=nn -> t
        | Var(_)                      as   t1    -> t1
        | Pattern(loc,f,lt)                      ->
          Pattern(loc,f,List.map (lift nk t) lt)
        | Lambda(loc,id,t1)                      ->
          Lambda(loc,id,lift nk t t1)
        | Brackets(t)                            ->
          assert false
    in
    if occurs nk t then false
    else
      solve f
        (List.map
           (fun (t1,t2,n) ->
              (lift nk t t1, lift nk t t2, n)
           ) l
        )
     
let unifiable : name -> pattern list -> pattern list -> bool = fun f t1 t2->
  try
    solve f
      (zip_with_int
         (List.map (name_var f 1 0) t1) (List.map (name_var f 2 0) t2) 0
      )
  with _ -> true

let rec cp_at_root : rule_infos list -> (rule_infos * rule_infos) option =
  function
  | []   -> None
  | a::l ->
     try
       Some (a, List.find (fun r -> unifiable a.cst a.args r.args) l)
     with
       Not_found -> cp_at_root l
