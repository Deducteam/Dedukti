open Types
open Printf

(*Code Generation*)

(* isType *)
let rec gen_code_istype ctx =
  function
  | Type                -> fprintf !Global.out "{ co = ctype }"
  | GVar (m,v)          -> 
      if Global.is_alias m v then fprintf !Global.out "app0(%s.%s_c)" m v
      else fprintf !Global.out "%s.%s_c" m v
  | Var v               -> 
      if List.mem v ctx then fprintf !Global.out "%s_c" v 
      else fprintf !Global.out "{ co=ctype } --[[ FIXME ]]" (*FIXME !!!!!*)
  | App (f,a)           -> 
      begin
        fprintf !Global.out  "app( " ;
        gen_code_istype ctx f ;
        fprintf !Global.out  " , " ;
        gen_code_istype ctx a ;
        fprintf !Global.out  " )"
      end
  | Lam (v,_,te)        -> 
      begin
        fprintf !Global.out "{ co = clam ; f = function (%s_c) return " v; 
        gen_code_istype (v::ctx) te ;
        fprintf !Global.out " end }"
      end
  | Pi (v0,ty,te)       ->
      let (arg,ctx') = match v0 with Some v -> (v^"_c",v::ctx) | None -> ("dummy",ctx) in
        begin
          fprintf !Global.out "{ co = cpi ; ctype = ";
          gen_code_istype ctx ty ;
          fprintf !Global.out " ; f = function (%s) return "  arg ;
          gen_code_istype ctx' te ;
          fprintf !Global.out " end }"
        end

let isType ctx = function
  | CTypeOf (m,v)       -> fprintf !Global.out "isType(%s.%s_c.ctype)\n" m v
  | CSub Type           -> ()
  | CSub s              -> raise (TypeSynthError (NotAType1 s))
  | CPi (v,a,b)         -> raise (TypeSynthError (NotAType2 (v,a,b)))
  | CDepType (f,a)      -> 
      begin
        (*FIXME est ce la peine d'appliquer vraiment a ?*)
        fprintf !Global.out "isType ( app( f_var_%i , " f ;
        gen_code_istype [] a ;
        fprintf !Global.out " ))\n" 
      end

(* ConvPi *)

let rec gen_code_class glo loc = function
  | Type                -> fprintf !Global.out "{ co = ctype }"
  | GVar (m,v)          -> 
      if Global.is_alias m v then fprintf !Global.out "app0(%s.%s_c)" m v
      else fprintf !Global.out "%s.%s_c" m v
  | Var v               -> 
      begin
        try 
          let _ = List.assoc v glo in 
            fprintf !Global.out "{ co=ctype --[[ FIXME ]] }" (*FIXME*)
            (*fprintf !Global.out "{ co=ccon ; id=\"%s\" ; ctype=" v;
            gen_class c;
            fprintf !Global.out " ; arity=0 ; f=function () return nil end ; args={ } }"*)
        with
          | Not_found ->
             if List.mem v loc then fprintf !Global.out "%s_c" v
             else fprintf !Global.out "%s_c --[[ FREEVAR ]]" v 
      end
  | App (f,a)           -> 
      begin
        fprintf !Global.out  "app( " ;
        gen_code_class glo loc f ;
        fprintf !Global.out  " , " ;
        gen_code_class glo loc a ;
        fprintf !Global.out  " )"
      end
  | Lam (v,_,te)        -> 
      begin
        fprintf !Global.out "{ co = clam ; f = function (%s_c) return " v; 
        gen_code_class glo (v::loc) te ;
        fprintf !Global.out " end }"
      end
  | Pi (v0,ty,te)       ->
      let (arg,loc') = match v0 with Some v -> (v^"_c",v::loc) | None -> ("dummy",loc) in
        begin
          fprintf !Global.out "{ co = cpi ; ctype = ";
          gen_code_class glo loc ty ;
          fprintf !Global.out " ; f = function (%s) return "  arg ;
          gen_code_class glo loc' te ;
          fprintf !Global.out " end }"
        end

let rec gen_class ctx = function
  | CTypeOf (m,v)       -> 
      if Global.is_alias m v then fprintf !Global.out "app0( %s.%s_c.ctype )" m v 
      else fprintf !Global.out "%s.%s_c.ctype" m v 
  | CSub s              -> gen_code_class ctx [] s
  | CPi (v,a,b)         ->
      begin
        fprintf !Global.out "{ co=cpi ; ctype=" ;
        gen_class ctx (CSub a);
        fprintf !Global.out " ; f=function (var_%s) return " v ;
        gen_class ((v,CSub a)::ctx) b;
        fprintf !Global.out " end }"
      end
  | CDepType (f,a)      ->
      begin
        fprintf !Global.out "app( f_var_%i , " f ;
        gen_class ctx (CSub a) ;
        fprintf !Global.out " )"
      end
                
let convPi ctx v pi a = 
  match pi with 
    | CSub Type                 -> raise (TypeSynthError ConvPi1)
    | CSub (Lam  (_,_,_))       -> raise (TypeSynthError ConvPi2)
    | CSub (Var _ )             -> assert false
    | _                         -> 
        begin
          fprintf !Global.out "f_var_%i = checkPi( " v ;
          gen_class ctx pi ;
          fprintf !Global.out " , ";
          gen_class ctx a ;
          fprintf !Global.out " )\n" 
        end

(* Type inference *)

type itype =
  | Kind
  | Class of classifier

let vvv = ref 0
let fresh_var _ = incr vvv ; !vvv 

let rec type_inference ctx : term -> itype = function
  | Type                -> Kind
  | GVar (m,id)         -> Class (CTypeOf (m,id))
  | Var v               -> 
      begin 
        try Class (List.assoc v ctx) 
        with Not_found -> raise (InternalError (ContextError (v,ctx)))
      end
  | Lam (_,None,_)      -> assert false
  | Lam (v,Some a,f)    -> 
      begin
        ( match type_inference ctx a  with
          | Kind      -> raise (TypeSynthError TypeInf0)
          | Class c   -> isType ctx c );
        match type_inference ((v,CSub a)::ctx) f  with
          | Kind      -> raise (TypeSynthError TypeInf1)
          | Class b   -> Class (CPi (v,a,b))           (* on a necessairement b:Type ou b:Kind *)
      end
  | Pi  (v0,a,b)        -> 
      begin
        ( match type_inference ctx a with
          | Kind        -> raise (TypeSynthError TypeInf2)
          | Class c     -> isType ctx c ) ;       
        let ctx' = match v0 with
          | None        -> ctx
          | Some v      -> (v,CSub a)::ctx
        in
          match type_inference ctx' b with
            | Kind        -> Kind 
            | Class c     -> ( isType ctx' c ; Class c )
      end
  | App (f,arg)         -> 
      begin
        let pi = match type_inference ctx f with
          | Kind        -> raise (TypeSynthError TypeInf3)
          | Class c     -> c          
        in
          match type_inference ctx arg with
            | Kind        -> raise (TypeSynthError TypeInf4)
            | Class a     -> 
                let v = fresh_var () in
                  ( convPi ctx v pi a ; Class (CDepType (v,arg)) )
      end

(* Entry Points *)

let typecheck_decl id loc ty = 
  fprintf !Global.out "print_debug(\"%s\tChecking declaration %s\t\t\")\n" (Debug.string_of_loc loc) id ;
  ( match type_inference [] ty with
    | Kind      -> ()
    | Class c   -> isType [] c ) ;
  fprintf !Global.out "print_ok()\n"

let typecheck_def id loc te ty = (*FIXME*) 
  fprintf !Global.out "print_debug(\"%s\tChecking definition %s\t\t\")\n" (Debug.string_of_loc loc) id ;
  ( match type_inference [] ty with
      | Kind          -> ()
      | Class c       -> isType [] c ) ;
  ( match type_inference [] te with
      | Kind          -> raise (TypeSynthError TypeCheckDef)
      | Class c       -> 
          begin 
            fprintf !Global.out "conv( " ;
            gen_class [] c ;
            fprintf !Global.out " , " ;
            CodeGeneration.gen_term ty ;
            fprintf !Global.out " )\n" 
          end
  ) ;
  fprintf !Global.out "print_ok()\n"

let apply f args = Array.fold_left (fun f a -> App (f,a) ) f args
let rec pat_to_term = function
  | Joker               -> assert false
  | Id id               -> Var id
  | Pat ((m,v),dots,pats)  -> apply (apply (GVar (m,v)) dots) (Array.map pat_to_term pats) 

let gen_env ((id,loc),te) = (*TODO type check env*) (*FIXME utiliser gen_code à la place*)
  fprintf !Global.out "local %s_c = { co = ccon ; id = \"%s\" ; ctype = " id id ; 
  gen_code_class [] [] te ;
  fprintf !Global.out " ; arity = 0 ; args = { } ; f = function() return nil end}\n" 

let typecheck_rule id i (loc,ctx,dots,pats,te) = 
  fprintf !Global.out "print_debug(\"%s\tChecking Rule %i of %s\t\t\")\n" (Debug.string_of_loc loc) (i+1) id ;
  fprintf !Global.out "do\n" ;
  List.iter gen_env ctx ;
  let le = pat_to_term (Pat ((!Global.name,id),dots,pats))     in
  let ctx2 = List.map (fun ((v,_),t) -> (v,CSub t))  ctx in
    ( match ( type_inference ctx2 le , type_inference ctx2 te ) with 
      | ( Kind , Kind )         -> ()
      | ( Class c1 , Class c2 ) ->
          begin
            fprintf !Global.out "conv( " ;
            gen_class ctx2 c1 ;
            fprintf !Global.out " , " ;
            gen_class ctx2 c2 ;
            fprintf !Global.out " )\n"
          end
      | ( _ , _ )               -> raise (TypeSynthError TypeCheckRule) ) ;
    fprintf !Global.out "end\n" ;
    fprintf !Global.out "print_ok()\n" 

let generate_decl id ty =
  fprintf !Global.out "%s.%s_c = { co=ccon ; id=\"%s.%s\" ; ctype = " !Global.name id !Global.name id; 
  gen_code_class [] [] ty ;
  fprintf !Global.out " ; arity=0 ; f = function () return nil end ; args = {} } "; 
  fprintf !Global.out "\n\n" 

let generate_def id te =
  fprintf !Global.out "%s.%s_def = " !Global.name id ;
  gen_code_class [] [] te ;
  fprintf !Global.out "\n\n" 
 
let generate_rules_code id rs =
  fprintf !Global.out "%s.%s_c = " !Global.name id ;
  CodeGeneration.generate_rules_code2 id rs ;
  fprintf !Global.out "\n" 
