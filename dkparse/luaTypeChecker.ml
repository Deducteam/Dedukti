open Types
open Lua_api
open Buffer

(* FIXME check for lua injection *)

let iteri f lst = 
  let i = ref 0 in
    List.iter (fun a -> f !i a ; incr i) lst

let rec iskind = function
  | Type          -> true
  | Pi (_,_,t)    -> iskind t
  | _             -> false

let force_true b =
  if b then () else assert false

let force_some = function
  | None        -> assert false
  | Some a      -> a
(*
let is_ok = function
  | Lua.LUA_OK  -> true
  | _           -> false
 *)
(*
let exec_buffer ls bf =   
  if LuaL.dostring ls (contents bf) then None
  else Some (force_some (Lua.tostring ls (-1)))
 *)
let exec_buffer_unsafe ls bf =   
  assert ( LuaL.dostring ls (contents bf) )

let print_status = function
  | Lua.LUA_OK              -> print_endline "OK"
  | Lua.LUA_YIELD           -> print_endline "YIELD"
  | Lua.LUA_ERRRUN          -> print_endline "ERRRUN"
  | Lua.LUA_ERRSYNTAX       -> print_endline "ERRSYNTAX"
  | Lua.LUA_ERRMEM          -> print_endline "ERRMEM"
  | Lua.LUA_ERRERR          -> print_endline "ERRERR"

let require ls dep =
  if LuaL.dostring ls ("require(\""^dep^"\")") then () (*FIXME*)
  else
   begin
     (*prerr_string (force_some (Lua.tostring ls (-1))) ;*)
     raise (TypeCheckingError(LuaRequireFailed dep))
   end

let init name = 
  Global.debug ("{Initialisation} "^name^"\t\t") ;
  let ls = LuaL.newstate () in
    LuaL.openlibs ls ;
    force_true (LuaL.dostring ls "require(\"jit\")") ;
    force_true (LuaL.dostring ls "require(\"dedukti\")") ;
(*    force_true (LuaL.dostring ls "require(\"profiler\")") ;*)
(*    force_true (LuaL.dostring ls ("profiler.start()") ) ;*)
    force_true (LuaL.dostring ls (name^" = { }")) ;
    List.iter (require ls) (List.rev !Global.libs) ; (*FIXME*)
    Global.debug_ok () ;
    ls
(*
let close ls =
  force_true (LuaL.dostring ls "profiler.stop()")
*)
let rec lua_code bf = function
  | Kind                -> add_string bf  "{ co = ckind }"
  | Type                -> add_string bf  "{ co = ctype }"
  | GVar v              -> add_string bf ("app0("^(!Global.name)^"."^v^"_c)")
  | EVar v              -> add_string bf ("app0("^v^"_c)")
  | Var v               -> add_string bf ("app0("^v^"_c)")
  | App (f,a)           -> ( add_string  bf  "app( " ; lua_code bf f ; add_string  bf  " , " ; lua_code bf a  ; add_string  bf  " )"    )
  | Lam (v,_,te)        -> ( add_string  bf ("{ co = clam ; f = function ("^v^"_c) return ") ; lua_code bf te ; add_string  bf " end }" )
  | Pi (v0,ty,te)       ->
      let arg = match v0 with Some v -> v^"_c" | None -> "dummy" in
        begin
          add_string  bf "{ co = cpi ; ctype = " ; lua_code bf ty ;
          add_string  bf (" ; f = function ("^arg^") return ") ;
          lua_code bf te ; add_string  bf " end }"
        end

let rec lua_term bf = function
  | Kind                -> assert false
  | Type                -> add_string bf "{ te = ttype }"
  | GVar v              -> add_string bf (!Global.name^"."^v^"_t")
  | EVar v              -> add_string bf (v^"_t")
  | Var v               -> add_string bf (v^"_t")
  | App (f,a)           -> 
      begin 
        add_string  bf "{ te = tapp ; f = " ; 
        lua_term    bf f ; 
        add_string  bf " ; a = " ; 
        lua_term    bf a ; 
        add_string  bf " ; ca = function() return " ; 
        lua_code    bf a ; 
        add_string  bf " end }"
      end
  | Lam (v,None,te)     -> 
      begin
        add_string  bf ("{ te = tlam ; ttype = nil ; ctype = nil ; f =  function ("^v^"_t, "^v^"_c) return "); 
        lua_term bf te;
        add_string  bf " end }";
      end
  | Lam (v,Some ty,te)  -> 
      begin
        add_string  bf "{ te = tlam ; ttype = ";
        lua_term bf ty ;
        add_string  bf " ; ctype = function() return " ;
        lua_code bf ty ;
        add_string  bf (" end ; f =  function ("^v^"_t, "^v^"_c) return ") ; 
        lua_term bf te;
        add_string  bf " end }";
      end
  | Pi  (ov,ty,t)       -> 
      let args = match ov with None -> "dummy1,dummy2" | Some v -> ( v^"_t,"^v^"_c" ) in
        begin 
          add_string  bf "{ te = tpi ; ttype = " ;
          lua_term bf ty ; 
          add_string  bf " ; ctype = function() return " ; 
          lua_code bf ty ;
          add_string  bf (" end ; f = function ("^args^") return ") ;
          lua_term bf t ;
          add_string  bf " end }"
      end

let check_type ls te ty = 
  let bf = create 80 in (*FIXME 80?*)
    add_string bf "type_check(0," ;
    lua_term bf te ;
    add_string bf "," ;
    lua_code bf ty ;
    add_string bf ")" ;
    (*print_endline (contents bf);*)
    if LuaL.dostring ls (contents bf) then ()
    else raise (TypeCheckingError(LuaTypeCheckingFailed (te,ty,force_some (Lua.tostring ls (-1)))))


let check_type_type ls ty =
  check_type ls ty (if iskind ty then Kind else Type)

let rec lua_pat_code bf = function
  | Id v                -> add_string bf (v^"_c")
  | Pat (c,dots,pats)   ->
      begin
        let first = ref true in
        let arity = Array.length dots + Array.length pats  in
        add_string bf "{ co = ccon ; id = \"";
        add_string bf (!Global.name^"."^c) ;
        add_string bf "\" ; arity = ";
        add_string bf (string_of_int arity);
        add_string bf " ; f = function() return ";
        add_string bf (!Global.name^"."^c);
        add_string bf "_c end ; args = { " ;
        Array.iter ( 
          fun t -> 
            if !first then ( lua_code bf t ; first := false )
            else ( add_string bf " ; " ; lua_code bf t )
        ) dots ;
        Array.iter ( 
          fun t -> 
            if !first then ( lua_pat_code bf t ; first := false )
            else ( add_string bf " ; " ; lua_pat_code bf t )
        ) pats ;
        add_string bf " } ; }"
      end

let rec lua_pat_term bf = function 
  | Id v                -> add_string bf (v^"_t")
  | Pat (c,dots,pats)   -> 
      let arity = Array.length dots + Array.length pats in
        for i=1 to arity do add_string bf " { te = tapp ; f = " done ;
        add_string bf (!Global.name^"."^c^"_t ") ;
        Array.iter (
          fun d -> 
            add_string bf " ; a = " ;
            lua_term bf d ;
            add_string bf " ; ca = function() return " ;
            lua_code bf d ;
            add_string bf " end } "
        ) dots ;
        Array.iter (
          fun p -> 
            add_string bf " ; a = " ;
            lua_pat_term bf p ;
            add_string bf " ; ca = function() return " ;
            lua_pat_code bf p ;
            add_string bf " end } "
        ) pats

let check_env bf (id,te) = 
  (if iskind te then add_string bf "chkkind(" else add_string bf "chktype(");
  lua_term   bf te ;
  add_string bf ")\nlocal " ;
  add_string bf id ;
  add_string bf "_c = { co = ccon ; id = \"" ;
  add_string bf id ;
  add_string bf "\" ; arity = 0 ; args = { } ; f = function() return nil end}\n" ;
  add_string bf "local " ;
  add_string bf id ;
  add_string bf "_t = { te = tbox, ctype = " ;
  lua_code   bf te ;
  add_string bf  "}\n"

let check_rule ls id (ctx,dots,pats,te) =
  let bf = create 80 in (*FIXME 80?*)
    List.iter (check_env bf) ctx ; 
    add_string bf "do\nlocal ty = type_synth(0, ";
    lua_pat_term bf (Pat (id,dots,pats)) ;
    add_string bf ")\nchk(";
    lua_term bf te ;
    add_string bf ", ty)\nend" ;
    if LuaL.dostring ls (contents bf) then ()
    else raise (TypeCheckingError(LuaRuleCheckingFailed (id,force_some (Lua.tostring ls (-1)))))

let add_in_context ls name ty = 
  let bf = create 80 in (*FIXME 80?*)
  (* name_c = code *)
    add_string bf name ;
    add_string bf "_c = { co = ccon ; id = \"" ; 
    add_string bf name ;
    add_string bf  "\" ; arity = 0 ; args = { } ; f = function() return nil end }\n" ;
  (* name_t = term *)
    add_string bf name ; 
    add_string bf "_t = { te = tbox ; ctype = " ;
    lua_code   bf ty ;
    add_string bf " } " ;
    (*print_endline (contents bf);*)
    exec_buffer_unsafe ls bf

let add_definition ls name te = 
  let bf = create 80 in (*FIXME 80?*)
  (* name_c = code *)
    add_string bf name ; 
    add_string bf "_c = " ;
    lua_code bf te ;
    add_string bf "\n" ;
  (* name_t = term *)
    add_string bf name ; 
    add_string bf "_t = " ;
    lua_term bf te ;
    exec_buffer_unsafe ls bf

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

let print_path bf p = 
    assert(p!=[]);
    iteri ( 
      fun i e ->
        if i=0 then add_string bf ("y"^(string_of_int (e+1))) 
        else add_string bf (".args["^(string_of_int (e+1))^"]")
    ) (List.rev p) (*FIXME*)

let print_locals bf vars locs = 
  assert (Array.length vars = Array.length locs);
  if Array.length vars = 0 then ()
  else 
    begin
      let first = ref true in
        add_string bf "local ";
        Array.iter (
          function 
            | Id id   -> if !first then ( add_string bf (id^"_c") ; first:=false) else add_string bf (", "^id^"_c")
            | _       -> assert false
        ) vars;
        first := true;
        add_string bf " = ";
        Array.iter (fun l -> (if !first then first:=false else add_string bf ", "  ) ; print_path bf l ) locs ;
        add_string bf "\n"
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

let rec cc bf id (pm:pMat) : unit =
  match getColumn pm.p.(0) with
    | None              -> 
        begin 
          let (ctx,te) = pm.a.(0) in
            print_locals bf pm.p.(0) pm.loc ;
            add_string bf "return ";
            lua_code bf te
        end
    | Some (c,n_id)     -> 
        begin
          assert (c < Array.length pm.loc);
          let bo = ref true in
            List.iter ( 
              fun (cst,arity,lst) ->
                (if !bo then bo := false else add_string bf "\nelse") ;
                add_string bf "if " ;
                print_path bf pm.loc.(c) ;
                add_string bf ".co == ccon and " ;
                print_path bf pm.loc.(c) ;
                add_string bf (".id == \""^(!Global.name)^"."^cst^"\" then\n") ;
                match specialize pm c arity lst with
                  | None        -> ( add_string bf "return " ; lua_code bf (snd pm.a.(0)) )
                  | Some pm'    -> ( cc bf n_id pm' )
            ) (partition pm.p c) ;

            (*DEFAULT*)
            add_string bf "\nelse\n";
            (match default pm c with
               | None           -> add_string bf "return nil"
               | Some pm'       -> ( cc bf n_id pm') 
            );
            add_string bf "\nend" 
        end 

let add_rules ls id rules = 
  assert ( Array.length rules > 0 );
  let gname = !Global.name^"."^id in     (*FIXME*)
  let (_,dots,pats,_) = rules.(0) in
  let arity = Array.length dots + Array.length pats in
  let bf = create 80 in (*FIXME 80?*)
    add_string bf gname ;
    add_string bf "_c = { co = ccon ; id=\"";
    add_string bf gname ;
    add_string bf "\" ; arity = ";
    add_string bf (string_of_int arity);
    add_string bf " ; args = { } ; f =\nfunction (" ;
    (if arity>0 then add_string bf "y1" else ());
    (for i=2 to arity do add_string bf (", y"^(string_of_int i))  done );
    add_string bf ")\n" ;
    cc bf id (new_pMat rules) ;
    add_string bf "\nend }\n\n" ;
    exec_buffer_unsafe ls bf

