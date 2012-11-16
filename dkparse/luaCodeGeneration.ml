open Types

  (* *********** Utils *********** *)

let iteri f lst = 
  let i = ref 0 in
    List.iter (fun a -> f !i a ; incr i) lst

let emit str   = output_string !Global.out str
let emit_int i = output_string !Global.out (string_of_int i)

(* *********** Prelude ********** *)

let prelude _ =
  emit "--[[ Code for module " ; emit !Global.name ; emit ". ]]\n" ;
  emit !Global.name ; emit " = { }\n\n"  


(* *********** Classifier Generation *********** FIXME *)

let rec get_vars a s = function
  | Lam (x,_,t)   -> get_vars (a+1) (s^", "^x^"_c") t
  | te            -> (a,s,te)

let rec gen_code = function
  | Type                -> emit "{ ck = ctype }"
  | GVar v              -> emit  (!Global.name^"."^v^"_c")
  | EVar v              -> emit  (v^"_c")
  | Var v               -> emit  (v^"_c")
  | App (f,a)           -> 
     begin
       emit "ap(" ; gen_code f ; emit ", " ; gen_code a ; emit ")" 
     end
  | Lam (v,_,te)        -> 
      begin
        let (arity,vars,te2) = get_vars 1 (v^"_c") te in (* uncurryfication *)
          emit "{ ck = clam, arity = " ; 
          emit_int arity ; 
          emit ", args = { }, clam = function (" ; 
          emit vars ; 
          emit ") return ";
          gen_code te2 ;
          emit " end }"
      end
  | Pi (v0,ty,te)       ->
      begin
        emit "{ ck = cpi, cpi = { ";
        gen_code ty ;
        ( match v0 with
            | Some v        -> ( emit ", function (" ; emit (v^"_c") ; emit ") return " )
            | None          ->   emit ", function (dummy_c) return "  );
        gen_code te ;
        emit " end } }"
      end 

(* *********** Subject Generation *********** FIXME *)

let rec gen_term = function
  | Type                -> emit "{ tk = ttype }"
  | GVar v              -> emit  (!Global.name^"."^v^"_t")
  | EVar v              -> emit  (v^"_t")
  | Var v               -> emit  (v^"_t")
  | App (f,a)           -> 
      begin 
        emit "{ tk = tapp, tapp = { " ; 
        gen_term f ; emit ", " ; 
        gen_term a ; emit ", " ; 
        gen_code a ; emit " } }"
      end
  | Lam (v,oty,te)      -> 
      begin
        emit "{ tk = tlam, tlam = { " ;
        ( match oty with
            | Some ty      -> gen_code ty  (*emit "nil" *)
            | None          -> emit "nil"   );
        emit (", function ("^v^"_t, "^v^"_c) return ") ; 
        gen_term te;
        emit " end } }";
      end
  | Pi  (ov,ty,t)       -> 
      begin 
        emit "{ tk = tpi; tpi = { " ;
        gen_term ty ; emit ", " ; gen_code ty ;
        ( match ov with
            | Some v    -> emit (", function ("^v^"_t, "^v^"_c) return ")
            | None      -> emit ", function (dummy_t, dummy_c) return " );
        gen_term t;
        emit " end } }"
      end

(* ************** Declarations *************** *)

let rec iskind = function
  | Type          -> true
  | Pi (_,_,t)    -> iskind t
  | _             -> false

let generate_decl_check gname ty =
  emit "--[[ Type checking " ; emit gname ; emit ". ]]\n" ;
  emit "chkbeg(\"" ; emit gname ; emit "\")\n" ;
  (if iskind ty then emit "chkkind(" else emit "chktype(") ;
  gen_term ty ;
  emit ")\nchkend(\"" ; emit gname ; emit "\")\n"

let generate_decl_code gname =
  emit gname ; emit "_c = { ck = ccon, ccon = \"" ; 
  emit gname ; emit "\", args = { } }\n"

let generate_decl_term gname ty =
  emit gname ; emit "_t = { tk = tbox, tbox = { " ;
  gen_code ty ;
  emit ", " ; emit gname ; emit "_c } }\n\n"

(* ***************** Pattern Matching Generation ************ *)

let new_pMat rules : pMat = 
    let rows = Array.length rules   in
      assert (rows>0);
      let (cols,nd) = match rules.(0) with (_,dots,pats,_) -> ( Array.length pats , Array.length dots ) in
        { p = Array.init rows (fun i -> let (_,_,pats,_) = rules.(i) in pats ) ; 
          a = Array.init rows (fun i -> let (ctx,_,_,ri) = rules.(i)   in (ctx,ri) ) ;
          loc = Array.init cols (fun i -> [i+nd]); 
          nb_dots = nd;
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
        let p = Array.make l_size (Array.make c_size (Id "")) in
        let a = Array.make l_size ([],Type) in
        let l = Array.make c_size [] in

          iteri (fun i k -> a.(i) <- pm.a.(k) ) lines;

          iteri (
            fun i k ->
              assert (k < Array.length pm.p && c < Array.length pm.p.(k));
              (match pm.p.(k).(c) with
                 | Id _              -> 
                     for j=0 to pred arity do
                       p.(i).(j) <- (Id "dummy") 
                     done
                 | Pat (_,_,pats)    ->
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

          for i=0 to pred arity     do l.(i) <- i::pm.loc.(c)           done;
          for i=0 to pred c         do l.(i+arity) <- pm.loc.(i)        done;
          for i=(c+1) to pred (Array.length pm.loc) do l.(i+arity-1) <- pm.loc.(i) done;
          
          Some { p=p ; a=a ; loc=l; nb_dots=pm.nb_dots; }
      end

let default (pm:pMat) (c:int) : pMat option = 
  try (
    let l_p = ref [] in
    let l_a = ref [] in
   (* let l_l = ref [] in*)
      for i=0 to pred (Array.length pm.p) do
        assert (c < Array.length pm.p.(i));
        match pm.p.(i).(c) with 
          | Id v  -> (
              l_p := pm.p.(i) :: !l_p;
              l_a := pm.a.(i) :: !l_a;
            (*  l_l := pm.loc.(i) :: !l_l;*)
            )
          | _     -> ()
      done ;
      if !l_p=[] then None
      else 
        Some { p = Array.of_list !l_p ; 
               a = Array.of_list !l_a ; 
               loc = pm.loc (*Array.of_list !l_l*); (*FIXME ??*)
               nb_dots = pm.nb_dots;
        } 
  ) with _ -> assert false

let print_path p = 
    assert(p!=[]);
    iteri ( fun i e ->
                   if i=0 then ( emit "y" ; emit_int (e+1) )
                   else ( emit ".args[" ; emit_int (e+1) ; emit "]" )
    ) (List.rev p) (*FIXME*)

let print_locals vars locs = 
  assert (Array.length vars = Array.length locs);
  if Array.length vars = 0 then ()
  else 
    begin
      let first = ref true in
        emit "local ";
        Array.iter (function 
                      | Id id   -> if !first then (emit id ; emit "_c" ; first:=false) else emit (", "^id^"_c") 
                      | _       -> assert false
        ) vars;
        first := true;
        emit " = ";
        Array.iter (fun l -> (if !first then first:=false else emit ", "  ) ; print_path l ) locs ;
        emit "\n"
      end
       
let getColumn arr =
    let rec aux i =
      if i < Array.length arr then
        match arr.(i) with
          | Id _                -> aux (i+1)
          | Pat (id,_,p)        -> Some (i,id)
            else None
    in aux 0

let partition (mx:pattern array array) (c:int) : (id*int*int list) list =
    let lst = ref [] in
    let checked = Array.make (Array.length mx) false in
      for i=0 to pred (Array.length mx) do
        if checked.(i) then ()
        else (
          assert (c < Array.length mx.(i));
          match mx.(i).(c) with
            | Id _              -> () 
            | Pat (cst,_,pats)   ->
                let l = ref [i] in
                  begin
                    for j=0 to pred (Array.length mx) do
                      match mx.(j).(c) with
                        | Id _              -> l := j::!l
                        | Pat (cst2,_,pats)    ->
                            if (cst=cst2 && i!=j) then ( l := j::!l ; checked.(j) <- true )
                            else ()
                    done ;
                    lst := (cst,Array.length pats,!l)::!lst ;
                    checked.(i) <- true
                  end
        )
      done ;
      !lst 

let rec cc id (pm:pMat) : unit =
  match getColumn pm.p.(0) with
    | None              -> 
        begin 
          let (ctx,te) = pm.a.(0) in
            print_locals pm.p.(0) pm.loc ;
            emit "return ";
            gen_code te
        end
    | Some (c,n_id)     -> 
        begin
          assert (c < Array.length pm.loc);
          let bo = ref true in
            List.iter ( 
              fun (cst,arity,lst) ->
                (if !bo then bo := false else emit "\nelse") ;
                emit "if " ;
                print_path pm.loc.(c) ;
                emit ".ck == ccon and " ;
                print_path pm.loc.(c) ;
                emit ".ccon == \"" ; emit (!Global.name^"."^cst) ; emit "\" then\n" ;   (*FIXME ??*)
                match specialize pm c arity lst with
                  | None        -> ( emit "return " ; gen_code (snd pm.a.(0)) ) (*FIXME ?? *)
                  | Some pm'    -> ( cc n_id pm' )
            ) (partition pm.p c) ;

            (*DEFAULT*)
            emit "\nelse\n";
            (match default pm c with
               | None           -> (
                   emit "return ";
                   emit "{ ck = ccon, ccon = \""; emit (!Global.name^"."^id) ; emit "\", args = { " ;
                   for i=1 to (Array.length pm.p.(0)+pm.nb_dots) do
                     if i=1 then emit "y1" else (emit ",y" ; emit_int i )
                   done;
                   emit " } }"
                 )
               | Some pm'       -> ( cc n_id pm') 
            );
            emit "\nend" 
        end 

(* ************** Rules *************** *)

let rec gpcode = function
  | Id v                -> emit (v^"_c")
  | Pat (c,dots,pats)   ->
      begin
        let arity = Array.length dots + Array.length pats -1 in
          for i=0 to arity do emit "ap(" done ;
          emit (!Global.name^"."^c^"_c");
          Array.iter ( fun t -> emit ", " ; gen_code t ; emit ")" ) dots ;
          Array.iter ( fun p -> emit ", " ; gpcode   p ; emit ")" ) pats
      end

let rec gpterm = function 
  | Id v                -> emit (v^"_t")
  | Pat (c,dots,pats)   ->
      begin
        let arity = Array.length dots + Array.length pats -1 in
          for i=0 to arity do emit "{ tk = tapp, tapp = { " done ;
          emit (!Global.name^"."^c^"_t") ;
          Array.iter ( fun t -> emit ", " ; gen_term t ; emit ", " ; gen_code t ; emit " } }" ) dots ;
          Array.iter ( fun p -> emit ", " ; gpterm   p ; emit ", " ; gpcode   p ; emit " } }" ) pats
      end

let gen_env (id,te) =
  emit ("chkbeg(\""^id^"\")\n") ;
  (if iskind te then emit "chkkind(" else emit "chktype(");
  gen_term te ;
  emit ")\nlocal " ; 
  emit id ; emit "_c = { ck = ccon, ccon = \"" ; emit id ; emit "\", args = { } }\nlocal " ;
  emit id ; emit "_t = { tk = tbox, tbox = { " ; gen_code te ; emit ", " ; emit id ; emit "_c } }\n"  ;
  emit ("chkend(\""^id^"\")\n")

let generate_rule_check id i (ctx,dots,pats,te) =
  emit ("chkbeg(\"rule " ^ (string_of_int (i+1)) ^ "\")\n") ;
  List.iter gen_env ctx ; 
  emit "do\nlocal ty = synth(0, ";
  gpterm (Pat (id,dots,pats));
  emit ")\nchk(";
  gen_term te ;
  emit (", ty)\nend\nchkend(\"rule " ^ (string_of_int (i+1)) ^ "\")\n")

let generate_rules_code id rules = 
  assert ( Array.length rules > 0 );
  let gname = !Global.name^"."^id in     (*FIXME*)
  let (_,dots,pats,_) = rules.(0) in
  let arity = Array.length dots + Array.length pats in
    emit ("--[[ Compiling rules of "^gname^". ]]\n") ;
    emit (gname ^ "_c = { ck = clam, arity = " ^ (string_of_int  arity) ^ ", args = { }, clam =\nfunction (y1") ;
    (for i=2 to arity do emit (", y" ^ (string_of_int i )) done );
    emit ")\n" ;
    cc id (new_pMat rules) ;
    emit "\nend }\n\n"

