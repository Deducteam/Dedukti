open Types
open Printf

  (* *********** Utils *********** *)

let iteri f lst = 
  let i = ref 0 in
    List.iter (fun a -> f !i a ; incr i) lst
 
(* *********** Lua Code Generation *********** *)

let rec gen_code = function
  | Type                -> fprintf !Global.out "{ ctype=true }"
  | GVar (_,m,v)        -> fprintf !Global.out "app0(dk_%s.%s_c)" m v
  | LVar (_,v)          -> fprintf !Global.out "%s_c" v 
  | App (f,a)           -> 
      begin
        fprintf !Global.out  "app( " ;
        gen_code f ;
        fprintf !Global.out  " , " ;
        gen_code a ;
        fprintf !Global.out  " )"
      end
  | Lam ((_,v),_,te)    -> 
      begin
        fprintf !Global.out "{ clam_f = function (%s_c) return " v ; 
        gen_code te ;
        fprintf !Global.out " end }"
      end
  | Pi (v0,ty,te)       ->
      let arg = match v0 with Some (_,v) -> v^"_c" | None -> "dummy" in
        begin
          fprintf !Global.out "{ cpi_cty = ";
          gen_code ty ;
          fprintf !Global.out " ; cpi_f = function (%s) return "  arg ;
          gen_code te ;
          fprintf !Global.out " end }"
        end

let rec gen_lazy_code = function
  | GVar (l,m,v)        -> fprintf !Global.out "{ clazy = function() return app0(dk_%s.%s_c) end }" m v
  | App (f,a) as c      -> 
      begin
        fprintf !Global.out  "{ clazy = function() return " ;
        gen_code c ;
        fprintf !Global.out  " end }"
      end
  | Pi (v0,ty,te)       ->
      let arg = match v0 with Some (_,v) -> v^"_c" | None -> "dummy" in
        begin
          fprintf !Global.out "{ cpi_cty = ";
          gen_lazy_code ty ;
          fprintf !Global.out " ; cpi_f = function (%s) return "  arg ;
          gen_code te ;
          fprintf !Global.out " end }"
        end
  | c                   -> gen_code c

(* *********** Lua Term Generation *********** *)

let rec gen_term = function
  | Type                -> fprintf !Global.out "{ ttype=true }"
  | GVar (_,m,v)        -> fprintf !Global.out "dk_%s.%s_t" m v
  | LVar (_,v)          -> fprintf !Global.out "%s_t" v
  | App (f,a)           -> 
      begin 
        fprintf !Global.out "{ tapp_f = " ; 
        gen_term f ; 
        fprintf !Global.out " ; tapp_a = " ; 
        gen_term a ; 
        fprintf !Global.out " ; tapp_ca = " ; 
        gen_lazy_code a ;
        fprintf !Global.out " }"
      end
  | Lam ((_,v),None,te)     -> 
      begin
        fprintf !Global.out "{ tlam_f =  function (%s_t, %s_c) return " v v ; 
        gen_term te;
        fprintf !Global.out " end }";
      end
  | Lam ((_,v),Some ty,te)  -> 
      begin
        fprintf !Global.out "{ tlam_tty = ";
        gen_term ty;
        fprintf !Global.out " ; tlam_cty = " ;
        gen_lazy_code ty ;
        fprintf !Global.out " ; tlam_f =  function (%s_t, %s_c) return " v v ; 
        gen_term te;
        fprintf !Global.out " end }";
      end
  | Pi  (ov,ty,t)       -> 
      let args = match ov with None -> "dummy1,dummy2" | Some (_,v) -> ( v^"_t,"^v^"_c" ) in
        begin 
          fprintf !Global.out "{ tpi_tty = " ;
          gen_term ty ; 
          fprintf !Global.out " ; tpi_cty = " ; 
          gen_lazy_code ty ;
          fprintf !Global.out " ; tpi_f = function (%s) return " args ;
          gen_term t;
          fprintf !Global.out " end }"
      end

(* ************** Declarations *************** *)

let rec iskind = function
  | Type          -> true
  | Pi (_,_,t)    -> iskind t
  | _             -> false

let generate_decl_check (loc,id) ty =
  fprintf !Global.out "\nprint_debug(\"%s\tChecking declaration %s\t\t\")\n" (Debug.string_of_loc loc) id ;
  (if iskind ty then fprintf !Global.out "chkkind(" else fprintf !Global.out "chktype(") ;
  gen_term ty ;
  fprintf !Global.out ")\n"

let generate_decl_code id =
  fprintf !Global.out "dk_%s.%s_c = { cid = \"%s.%s\" ; args = { } }\n" !Global.name id !Global.name id

let generate_decl_term id ty =
  fprintf !Global.out "dk_%s.%s_t = { tbox_cty = " !Global.name id ;
  gen_lazy_code ty ;
  fprintf !Global.out " }\n" 

(* ************** Definitions *************** *)

let generate_def_check (l,id) te ty = 
  fprintf !Global.out "\nprint_debug(\"%s\tChecking definition %s\t\t\")\n" (Debug.string_of_loc l) id ;
  fprintf !Global.out "chk( " ;
  gen_term te ;
  fprintf !Global.out " , " ;
  gen_code ty ;
  fprintf !Global.out ")\n"

let generate_def_term id te = 
  fprintf !Global.out "dk_%s.%s_t = " !Global.name id ;
  gen_term te ;
  fprintf !Global.out "\n"

let generate_def_code id te = 
  fprintf !Global.out "dk_%s.%s_c = " !Global.name id ;
  (*gen_lazy_code te ;*) gen_code te ;
  fprintf !Global.out "\n"

(* ***************** Pattern Matching Generation ************ *)

let print_path p = 
    assert(p!=[]);
    iteri ( 
      fun i e ->
        if i=0 then fprintf !Global.out "y%i" (e+1) 
        else fprintf !Global.out ".args[%i]" (e+1) 
    ) (List.rev p) (*get rid of rev?*)

let print_locals vars locs = 
  assert (Array.length vars = Array.length locs);
  if Array.length vars = 0 then ()
  else 
    begin
      let first = ref true in
        fprintf !Global.out "local ";
        Array.iter (
          function 
            | Var (_,id)        -> if !first then (fprintf !Global.out "%s_c" id ; first:=false) else fprintf !Global.out ", %s_c" id 
            | Joker             ->  if !first then (fprintf !Global.out "dummy" ; first:=false) else fprintf !Global.out ", dummy" 
            | _                 -> assert false
        ) vars;
        first := true;
        fprintf !Global.out " = ";
        Array.iter (fun l -> (if !first then first:=false else fprintf !Global.out ", "  ) ; print_path l ) locs ;
        fprintf !Global.out "\n"
      end

let rec print_gdt = function
  | LeafNil             -> fprintf !Global.out "return nil"
  | Leaf1 (p,l,te)      -> 
      begin
        print_locals p l ;
        fprintf !Global.out "return ";
        gen_code te
      end
  | Leaf2 te            -> ( fprintf !Global.out "return " ; gen_code te )
  | Node ([] ,def)      -> assert false (*???*)
  | Node (fst::lst,def) -> 
      begin
        (*CASES*)
        let print_case (l,(m,cst),tr) =
          fprintf !Global.out "if " ;
          print_path l ;
          fprintf !Global.out ".cid == \"%s.%s\" then\n" m cst ; 
          print_gdt tr ;
        in
          print_case fst ; 
          List.iter (fun c -> fprintf !Global.out "\nelse" ; print_case c) lst ;
        (*DEFAULT*)
        fprintf !Global.out "\nelse\n";
        print_gdt def ;
        fprintf !Global.out "\nend"
      end


(* ************** Rules *************** *)

let rec gpcode = function
  | Joker                       -> assert false (*TODO*)
  | Var (_,v)                   -> fprintf !Global.out "%s_c" v
  | Pat ((l,m,c),dots,pats)       ->
      begin
        (*let first = ref true in*)
        let arity = Array.length dots + Array.length pats  in
          if arity = 0 then
            fprintf !Global.out "app0(dk_%s.%s_c) " m c
          else
            begin 
              for i=1 to arity do fprintf !Global.out "app( " done ;
              fprintf !Global.out "app0(dk_%s.%s_c)" m c ; 
              Array.iter ( 
                fun t -> 
                  fprintf !Global.out " , " ; 
                  gen_code t ; 
                  fprintf !Global.out " ) " 
              ) dots ;
              Array.iter ( 
                fun t -> 
                  fprintf !Global.out " , " ; 
                  gpcode t ; 
                  fprintf !Global.out " ) " 
              ) pats ;
            end
      end

let rec gpterm = function 
  | Joker                       -> assert false (*TODO*)
  | Var (_,v)                   -> fprintf !Global.out "%s_t" v
  | Pat ((l,m,c),dots,pats)     -> 
      let arity = Array.length dots + Array.length pats in
        for i=1 to arity do fprintf !Global.out " { tapp_f = " done ;
        fprintf !Global.out "dk_%s.%s_t " m c ;
        Array.iter (
          fun d -> 
            fprintf !Global.out " ; tapp_a = " ;
            gen_term d ;
            fprintf !Global.out " ; tapp_ca = " ;
            gen_lazy_code d ;
            fprintf !Global.out " } "
        ) dots ;
        Array.iter (
          fun p -> 
            fprintf !Global.out " ; tapp_a = " ;
            gpterm p ;
            fprintf !Global.out " ; tapp_ca = { clazy = function() return " ; (*gpcode_lazy*)
            gpcode p ;
            fprintf !Global.out " end } } "
        ) pats

(* Env *)

let gen_env ( (loc,id),te : var*term ) =
  fprintf !Global.out "\nprint_debug(\"%s\tChecking variable %s\t\t\")\n" (Debug.string_of_loc loc) id ;
  (if iskind te then fprintf !Global.out "chkkind(" else fprintf !Global.out "chktype(");
  gen_term te ;
  fprintf !Global.out ")\n%s_c = { cid = \"%s\" ; args = { } }\n" id id ; 
  fprintf !Global.out "%s_t = { tbox_cty = " id ;
  gen_lazy_code te ;
  fprintf !Global.out " }\n" 

(* Rules*)

let generate_rule_check i (ctx,((l,id),dots,pats),te : rule) =
  fprintf !Global.out "\ndo";
  List.iter gen_env ctx ; 
  fprintf !Global.out "\nprint_debug(\"%s\tChecking rule %i for %s.%s\t\t\")\n" (Debug.string_of_loc l) (i+1) !Global.name id ;
  fprintf !Global.out "local ty = type_synth( ";
  gpterm (Pat ((l,!Global.name,id),dots,pats));
  fprintf !Global.out ")\n";
  fprintf !Global.out "chk( ";
  gen_term te ;
  fprintf !Global.out ", ty )\nend\n"

let generate_rules_code (rules:rule array) = 
  (*assert ( Array.length rules > 0 );*)
  let (env,((l,id),dots,pats),te) = rules.(0) in
  let arity = Array.length dots + Array.length pats in
    if arity=0 then
      begin
        fprintf !Global.out "dk_%s.%s_c = { cid=\"%s.%s\" ; arity = 0 ; args = { } ; f = function() return " !Global.name id !Global.name id ;
        gen_code te ;
        fprintf !Global.out " end }\n"
      end
    else
      begin
        fprintf !Global.out "dk_%s.%s_c = { cid=\"%s.%s\" ; arity = %i ; args = { } ; f = function(" !Global.name id !Global.name id arity ;
        fprintf !Global.out "y1" ;
        (for i=2 to arity do fprintf !Global.out ", y%i" i  done );
        fprintf !Global.out ")\n" ;
        (for i=1 to arity do fprintf !Global.out "local y%i = force2(y%i)\n" i i done );
        print_gdt (PatternMatching.compute_gdt rules) ;
        fprintf !Global.out "\nend }\n" 
      end
 
(* External symbol checks *)

let rec ext_check_term = function
  | Type | LVar _                               -> ()
  | Lam (_,None,a)                              -> ext_check_term a
  | App (a,b) | Lam (_,Some a,b) | Pi (_,a,b)   -> ( ext_check_term a ; ext_check_term b )
  | GVar (_,m,_)                                -> if Scope.is_checked m then fprintf !Global.out "check_ext(dk_%s,'[ Lua ]  %s is undefined.')\n" m m

let rec ext_check_pat = function
  | Joker | Var _        -> ()
  | Pat (_,te,pats)     -> ( Array.iter ext_check_term te ; Array.iter ext_check_pat pats )

let ext_check_rule (env,(_,dots,pats),te) =
  List.iter (fun e -> ext_check_term (snd e)) env ;
  Array.iter ext_check_term dots ;
  Array.iter ext_check_pat pats ;
  ext_check_term te
 
(* Entry Points *)
 
let mk_require0 dep = 
  fprintf !Global.out "require(\"%s\")\n" dep 

let mk_require loc dep = 
  Global.debug (Debug.string_of_loc loc ^ "\tGenerating dependency "^dep^" \t\t") ;
  mk_require0 dep ;
  Global.debug_ok ()

let mk_prelude (l,name) =
  Global.debug (Debug.string_of_loc l ^ "\tGenerating prelude for module " ^ name ^ "\t\t") ;
  if !Global.lua_path <> "" then fprintf !Global.out "package.path = '%s/?.lua;' .. package.path \n" !Global.lua_path  ;
  fprintf !Global.out "require('dedukti')\n" ;
  List.iter mk_require0 !Global.libs ;
  fprintf !Global.out "dk_%s = { }\n" name ; 
  if not !Global.do_not_check then fprintf !Global.out "debug_infos = %B\n" (not !Global.quiet) ;
  Global.debug_ok ()
      
let mk_declaration ( v,ty0 : var * pterm ) =
  Global.debug (Debug.string_of_loc (fst v) ^ "\tGenerating declaration " ^ (!Global.name) ^ "." ^ (snd v) ^ "\t\t") ;
  let ty = Scope.check_scope ty0 in
    if Scope.gscope_add_decl v then
      begin
        if !Global.check_ext then ext_check_term ty  ;
        if not !Global.do_not_check then generate_decl_check v ty  ;
        generate_decl_code (snd v) ;
        generate_decl_term (snd v) ty ;
        Global.debug_ok ()
      end
    else
  Global.debug_ig ()

let mk_definition = function
  | Def    (v,ty0,te0)  ->  
      begin
        Global.debug (Debug.string_of_loc (fst v) ^ "\tGenerating definition " ^ (snd v) ^ "\t\t") ;
        let ty = Scope.check_scope ty0 in
        let te = Scope.check_scope te0 in
          Scope.gscope_add v ;
          ( if !Global.check_ext then ext_check_term te ) ;
          ( if not !Global.do_not_check then ( ext_check_term ty ; generate_def_check v te ty ) ) ;
          generate_def_code (snd v) te ;
          generate_def_term (snd v) te ;
          Global.debug_ok () 
      end
  | Opaque (v,ty0,te0)  -> 
      begin
        Global.debug (Debug.string_of_loc (fst v) ^ "\tGenerating opaque definition " ^ (snd v) ^ "\t\t") ;
        let ty = Scope.check_scope ty0 in
        let te = Scope.check_scope te0 in
          Scope.gscope_add v ;
          ( if !Global.check_ext then ext_check_term ty ) ;
          ( if not !Global.do_not_check then ( ext_check_term te ; generate_def_check v te ty ) ) ;
          generate_decl_code (snd v) ;
          generate_decl_term (snd v) ty ;
          Global.debug_ok () 
      end
  | Anon   (l,ty0,te0)  -> 
      begin
        Global.debug (Debug.string_of_loc l ^ "\tGenerating typechecking ... \t\t") ;
        let ty = Scope.check_scope ty0 in
        let te = Scope.check_scope te0 in
          ( if !Global.check_ext then ( ext_check_term ty ; ext_check_term te ) ) ;
          ( if not !Global.do_not_check then generate_def_check (l,"_") te ty ) ;
          Global.debug_ok () 
      end

let get_var = function
  | []                  -> assert false
  | (_,(v,_,_),_)::_    -> v

let mk_rules ( rs0 : prule list ) =
  let (loc,id) = get_var rs0 in
  Global.debug (Debug.string_of_loc loc ^ "\tGenerating rule checks for "^ id ^" \t\t") ; 
  let rs = Scope.check_scope_rules rs0 in
    if !Global.check_ext        then List.iter ext_check_rule rs ;
    if not !Global.do_not_check then iteri generate_rule_check rs ;
    generate_rules_code (Array.of_list rs) ;
    Global.debug_ok () 

let mk_ending _ =  
  (* if not !Global.quiet then fprintf !Global.out "\nprint_debug(\"SUCCESS
   * !\\n\")\n" ;*) 
  Global.debug " --------- \n"

(* let mk_error _ = fprintf !Global.out "\nos.exit(1)\n" *)
