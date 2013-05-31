open Types

let iteri f lst = 
  let i = ref 0 in
    List.iter (fun a -> f !i a ; incr i) lst

type pMat = { p:pat array array ; a:((var*term) list *term) array ; loc:occ array ; }

(*
 * type top_pat = id * term array * pat array
* type rule  = ( var * term ) list * top_pat * term  
* *)

let rec dots_to_joker : pat -> pat = function
  | Pat (id,dots,pats)  -> 
      let d = Array.length dots in
      let pats2 = Array.init (d+Array.length pats) (
        fun i ->
          if i<d then Joker 
          else dots_to_joker (pats.(i-d))
      ) in
        Pat (id,[||],pats2)
  | p                   -> p
 
let new_pMat (rules:rule array) : pMat = 
  let rows = Array.length rules   in
  (*  assert (rows>0); *)
  let cols = match rules.(0) with (_,(_,dots,pats),_) -> Array.length dots + Array.length pats in
    { p   = Array.init rows 
              (fun i ->
                 let (_,(_,dots,pats),_) = rules.(i) in
                 let nd = Array.length dots in
                   Array.init cols (fun j -> if j<nd then Joker else dots_to_joker pats.(j-nd) )
              ) ; 
      a   = Array.init rows (fun i -> let (ctx,_,ri) = rules.(i)   in (ctx,ri) ) ;
      loc = Array.init cols (fun i -> [i]); 
    }

let specialize (pm:pMat) (c:int) (arity:int) (lines:int list) : pMat option = 
  assert (0 < Array.length pm.p);
  assert (c < Array.length pm.p.(0));
    
  let l_size = List.length lines                in                                                                                   
  let c_size = (Array.length pm.p.(0))+arity-1  in
    if c_size=0 then None
    else
      begin
        assert (l_size <= Array.length pm.p);
        let p = Array.init l_size (fun _ -> Array.make c_size (Var ((0,0),"dummy"))) in
        let a = Array.make l_size ([],Type) in
        let l = Array.make c_size [] in
          
          iteri (fun i k -> a.(i) <- pm.a.(k) ) lines;

          iteri (
            fun i k ->
              assert (k < Array.length pm.p && c < Array.length pm.p.(k));
              (match pm.p.(k).(c) with
                 | Joker                -> ()
                 | Var _                 -> ()
                 | Pat (_,_,pats)       ->
                     for j=0 to (arity-1) do
                       p.(i).(j) <- pats.(j)
                     done
              );
              for j=0 to pred c do
                p.(i).(arity+j) <- pm.p.(k).(j) 
              done;
              for j=(c+1) to pred (Array.length pm.p.(k)) do
                let tmp =pm.p.(k).(j) in
                  p.(i).(arity+j-1) <-  tmp
              done 
          ) lines; 

          for i=0 to pred arity     do l.(i) <- i::pm.loc.(c)                 done;
          for i=0 to pred c         do l.(i+arity) <- pm.loc.(i)                        done;
          for i=(c+1) to pred (Array.length pm.loc) do l.(i+arity-1) <- pm.loc.(i)      done;
          
          Some { p=p ; a=a ; loc=l; }
      end

let default (pm:pMat) (c:int) : pMat option = 
    let l_p = ref [] in
    let l_a = ref [] in
      for i=0 to pred (Array.length pm.p) do
        assert (c < Array.length pm.p.(i));
        match pm.p.(i).(c) with 
          | Joker | Var _  -> (
              l_p := pm.p.(i) :: !l_p;
              l_a := pm.a.(i) :: !l_a;
            )
          | _     -> ()
      done ;
      if !l_p=[] then None
      else 
        Some { p = Array.of_list !l_p ; 
               a = Array.of_list !l_a ; 
               loc = pm.loc ; 
        } 

let partition (mx:pat array array) (c:int) : ((string*string)*int*int list) list =
    let lst = ref [] in
    let checked = Array.make (Array.length mx) false in
      for i=pred (Array.length mx) downto 0 do
        if checked.(i) then ()
        else (
          assert (c < Array.length mx.(i));
          match mx.(i).(c) with
            | Joker                     -> () 
            | Var _                     -> () 
            | Pat ((_,m,cst),_,pats)    ->
                let l = ref [] in
                  begin
                    for j=0 to pred (Array.length mx) do
                      match mx.(j).(c) with
                        | Joker             -> l := j::!l
                        | Var _             -> l := j::!l
                        | Pat ((_,m',cst'),_,pats)    ->
                            if (cst=cst' && m=m' && i!=j) then ( l := j::!l ; checked.(j) <- true )
                            else ()
                    done ;
                    lst := ((m,cst),Array.length pats,i::!l)::!lst ;
                    checked.(i) <- true
                  end
        )
      done ;
      !lst 

let getColumn arr =
    let rec aux i =
      if i < Array.length arr then
        match arr.(i) with
          | Joker               -> aux (i+1)
          | Var _               -> aux (i+1)
          | Pat (id,_,p)        -> Some (i,id)
            else None
    in aux 0

let rec cc (pm:pMat) : gdt =
  match getColumn pm.p.(0) with
    | None              -> Leaf1 ( pm.p.(0) , pm.loc , snd (pm.a.(0)) )
    | Some (c,_)     -> 
        begin
          assert (c < Array.length pm.loc);
          let par = partition pm.p c in
          let cases =
            List.map ( 
              fun (id,arity,lst) ->
                  match specialize pm c arity lst with
                    | None        -> ( pm.loc.(c) , id , Leaf2 ( snd pm.a.(match lst with z::_ -> z | _ -> assert false) ) )
                    | Some pm'    -> ( pm.loc.(c) , id , cc pm' )
            ) par in
          let def =
            (match default pm c with
               | None           -> LeafNil
               | Some pm'       -> cc pm'
            ) in
            Node (cases,def)
        end 

let compute_gdt rules = cc (new_pMat rules)
