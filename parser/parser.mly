%parameter <M :
  sig
    val mk_prelude     : Basic.loc -> Basic.ident -> unit
    val mk_declaration : Basic.loc -> Basic.ident -> Signature.staticity -> Term.term -> unit
    val mk_definition  : Basic.loc -> Basic.ident -> Term.term option -> Term.term -> unit
    val mk_opaque      : Basic.loc -> Basic.ident -> Term.term option -> Term.term -> unit
    val mk_rules       : Rule.untyped_rule list -> unit
    val mk_command     : Basic.loc -> Cmd.command -> unit
    val mk_ending      : unit -> unit
  end>
%{
    open Basic
    open Preterm
    open Scoping
    open Rule
    open Cmd
    open M

    let fresh_var var_list var =
      if List.mem var var_list then
        (* Split the variable name in a string part and a number part *)
        let s = string_of_ident var in
        let number_part = ref "" in
        let i = ref (String.length s - 1) in
        while s.[!i] >= '0' && s.[!i] <= '9' do
          number_part := Printf.sprintf "%c%s" s.[!i] !number_part;
          i := !i - 1
        done;
        let name_part = String.sub s 0 (!i+1) in
        assert (s = name_part ^ !number_part);
        let int_part =
          ref (try
                  int_of_string !number_part
                with Failure "int_of_string" -> 0)
        in
        while List.mem (hstring (Printf.sprintf "%s%d" name_part !int_part)) var_list do
          int_part := !int_part + 1
        done;
        hstring (Printf.sprintf "%s%d" name_part !int_part)
      else
        var

    let rec pre_subst map = function
      | PreId (_, x) as t -> (try List.assoc x map with Not_found -> t)
      | PreQId _
      | PreType _ as t -> t
      | PreApp (f, a, l) -> PreApp (pre_subst map f,
                                   pre_subst map a,
                                   List.map (pre_subst map) l)
      | PrePi (l, None, ty, body) ->
         PrePi (l, None, pre_subst map ty, pre_subst map body)
      | PreLam (l, x, ty, body) ->
         let x' = fresh_var (List.map fst map) x in
         let map' = (x, PreId (l, x')) :: map in
         let ty' =
           match ty with
           | None -> None
           | Some ty -> Some (pre_subst map' ty)
         in
         PreLam (l, x', ty', pre_subst map' body)
      | PrePi (l, Some x, ty, body) ->
         let x' = fresh_var (List.map fst map) x in
         let map' = (x, PreId (l, x')) :: map in
         PrePi (l, Some x', pre_subst map' ty, pre_subst map' body)

    let mk_let var term body = pre_subst [(var, term)] body

    let rec mk_lam (te:preterm) : param list -> preterm = function
        | [] -> te
        | PDecl(l,x,ty)::tl -> PreLam (l,x,Some ty,mk_lam te tl)
        | PDef(l,x,t)::tl -> mk_let x t (mk_lam te tl)

    let rec mk_pi (te:preterm) : param list -> preterm = function
        | [] -> te
        | PDecl(l,x,ty)::tl -> PrePi(l,Some x,ty,mk_pi te tl)
        | PDef(l,x,t)::tl -> mk_let x t (mk_pi te tl)

    let rec preterm_loc = function
        | PreType l | PreId (l,_) | PreQId (l,_,_) | PreLam  (l,_,_,_)
        | PrePi   (l,_,_,_) -> l
        | PreApp (f,_,_) -> preterm_loc f

    let mk_pre_from_list = function
        | [] -> assert false
        | [t] -> t
        | f::a1::args -> PreApp (f,a1,args)

    let rec filter_map f = function
      | [] -> []
      | a :: l ->
         (match f a with
          | None -> filter_map f l
          | Some b -> b :: filter_map f l)

    let mk_record_type (lt, ty_name) (params : param list) (lc, constr) (fields : pfield list) =
      (* Convert params and fields to decls, patterns and terms *)
      let preid (l, x) = PreId (l, x) in
      let pdecl (l,x,ty) = PDecl(l,x,ty) in
      let p_as_decl = function PDecl (l,x,_) -> Some (l,x) | PDef _ -> None in
      let p_as_pattern (l, _) = PJoker l in
      let f_as_pattern f (l, x, _) = if x == f then PPattern (l,None,x,[]) else PJoker l in
      let params_as_decls = filter_map p_as_decl params in
      let params_as_patterns = List.map p_as_pattern params_as_decls in
      let fields_as_patterns f = List.map (f_as_pattern f) fields in
      let params_as_terms = List.map preid params_as_decls in

      let params_and_fields = params @ List.map pdecl fields in

      let rec_type = mk_pre_from_list (PreId (lt, ty_name) :: params_as_terms) in
      let rec_name = hstring "record" in
      let field_proj_id n = let s = string_of_ident n in hstring ("proj_" ^ s) in
      let field_proj_preterm (l, n, t) =
        mk_pre_from_list (PreId(l, field_proj_id n) :: params_as_terms @ [PreId(lt, rec_name)])
      in
      let field_proj_subst (l, n, t) = (n, field_proj_preterm (l, n, t)) in
      let param_and_field_patterns f = params_as_patterns @ fields_as_patterns f in
      (* Declare the record type *)
      mk_declaration lt ty_name Signature.Static (scope_term [] (mk_pi (PreType lt) params));
      (* Declare the constructor *)
      mk_declaration lc constr Signature.Static (scope_term [] (mk_pi rec_type (params_and_fields)));
      List.iter
        (fun (lf, field_name, field_type) ->
          let proj_id = field_proj_id field_name in
          (* Declare the projection as definable *)
          mk_declaration lf proj_id Signature.Definable
            (scope_term []
               (mk_pi
                  (pre_subst (List.map field_proj_subst fields) field_type)
                  (params @ [PDecl(lf, rec_name, rec_type)])));
          (* Define the projection *)
          mk_rules [
            scope_rule (
              lf,
              None,
              [(lf,field_name)],
              None,
              proj_id,
              params_as_patterns @
                [PPattern (lf,
                           None,
                           constr,
                           param_and_field_patterns field_name)],
              PreId (lf, field_name))])
        fields

      (* contains every modules that are declared in the current environement*)	
      let modules = ref []
      (* contains only the modules that are not closed *)
      let current_modules = ref []
      (* to each module, a level is associated *)
      let level = ref 0

      (* called on the NEWMODULE declaration *)
      let mk_module _ m =
          modules :=  (m,!level)::!modules;
	  current_modules := m::!current_modules;
	  incr level

      (* called on the ENDMODULE declaration *)
      let umk_module _ =
      	  assert (List.length !modules <> 0);
       	  assert (List.length !current_modules <> 0);
	  current_modules := List.tl (!current_modules);
	  let mds = List.filter (fun (md,l) -> l < !level ) !modules in
	  modules := mds;
	  decr level

      (* prefix the ident id by the list of modules mds *)	  	  
      let rec prefix_by_mds mds id =
     	  match mds with   
	  | [] -> id
	  | x::t ->
	    hstring ((string_of_ident x) ^ "__" ^ string_of_ident (prefix_by_mds t id))

      (* for a module md, find in which modules it is defined. For example
      	 with #NEWMODULE D. #NEWMODULE E. prefix_of_md E is D *)
      let rec prefix_of_md md =
      	  let rec aux p mds =
      	      match mds with
	      	  | [] -> []
		  | (x,l)::t -> let p' = List.filter (fun (md,l') -> l' < l) p in
		    	     	if ident_eq x md then p'
				else aux ((x,l)::p') t 		    	     
	  in
	  List.map fst (aux [] (List.rev !modules))

      (* transform a qid in a old qid of dedukti *)	  	  
      let of_qid (loc, mds, id) =
      	  match mds with
	  | [] -> assert false
	  | x::t -> if List.exists (fun (md,_) -> Basic.ident_eq x md) !modules then
	    	        (loc, !Scoping.name, prefix_by_mds ((prefix_of_md x)@mds) id)
		    else (
			(loc, x, prefix_by_mds t id))

      (* update prefixes to match new modules declarations *)		
      let of_id id = prefix_by_mds (List.rev !current_modules) id

      let of_lid lid = (fst lid, of_id (snd lid))
%}

%token EOF
%token DOT
%token COMMA
%token COLON
%token ARROW
%token FATARROW
%token LONGARROW
%token DEF
%token LEFTPAR
%token RIGHTPAR
%token LEFTBRA
%token RIGHTBRA
%token LEFTSQU
%token RIGHTSQU
%token RECORD
%token <Basic.loc> WHNF
%token <Basic.loc> HNF
%token <Basic.loc> SNF
%token <Basic.loc> STEP
%token <Basic.loc> INFER
%token <Basic.loc> CONV
%token <Basic.loc> CHECK
%token <Basic.loc> PRINT
%token <Basic.loc> GDT
%token <Basic.loc*string> OTHER
%token <Basic.loc> UNDERSCORE
%token <Basic.loc*Basic.ident>NAME
%token <Basic.loc*Basic.ident>NEWMODULE
%token <Basic.loc>ENDMODULE
%token <Basic.loc> TYPE
%token <Basic.loc> KW_DEF
%token <Basic.loc> KW_THM
%token <Basic.loc> KW_INJ
%token <Basic.loc*Basic.ident> ID
%token <Basic.loc*Basic.ident list*Basic.ident> QID
%token <Basic.loc*string> STRING
%token <Basic.loc*string> NUM
%token <Basic.loc*char> CHAR

%start prelude
%start line
%type <unit> prelude
%type <unit> line
%type <Preterm.prule> rule
%type <Preterm.pdecl> decl
%type <Preterm.param> param
%type <Preterm.pdecl list> context
%type <Basic.loc*Basic.ident option*Basic.ident*Preterm.prepattern list> top_pattern
%type <Preterm.prepattern> pattern
%type <Preterm.prepattern> pattern_wp
%type <Preterm.preterm> sterm
%type <Preterm.preterm> term

%%

prelude         : NAME DOT      { let (lc,name) = $1 in
                                        Pp.name := name;
                                        Scoping.name := name;
                                        mk_prelude lc name }

line            : ID COLON letterm DOT
                { mk_declaration (fst $1) (of_id (snd $1)) Signature.Static (scope_term [] $3) }
                | ID param+ COLON letterm DOT
                { mk_declaration (fst $1) (of_id (snd $1)) Signature.Static (scope_term [] (mk_pi $4 $2)) }
                | KW_DEF ID COLON arrterm DOT
                { mk_declaration (fst $2) (of_id (snd $2)) Signature.Definable (scope_term [] $4) }
                | KW_INJ ID COLON arrterm DOT
                { mk_declaration (fst $2) (of_id (snd $2)) Signature.Injective (scope_term [] $4) }
                | KW_DEF ID COLON arrterm DEF letterm DOT
                { mk_definition (fst $2) (of_id (snd $2)) (Some (scope_term [] $4)) (scope_term [] $6) }
                | KW_DEF ID DEF letterm DOT
                { mk_definition (fst $2) (of_id (snd $2))  None (scope_term [] $4) }
                | KW_DEF ID param+ COLON arrterm DEF letterm DOT
                { mk_definition (fst $2) (of_id (snd $2)) (Some (scope_term [] (mk_pi $5 $3)))
                        (scope_term [] (mk_lam $7 $3)) }
                | KW_DEF ID param+ DEF letterm DOT
                { mk_definition (fst $2) (of_id (snd $2)) None (scope_term [] (mk_lam $5 $3)) }
                | KW_THM ID COLON arrterm DEF letterm DOT
                { mk_opaque (fst $2) (of_id (snd $2)) (Some (scope_term [] $4)) (scope_term [] $6) }
                | KW_THM ID DEF letterm DOT
                { mk_opaque (fst $2) (of_id (snd $2))  None (scope_term [] $4) }
                | KW_THM ID param+ COLON arrterm DEF letterm DOT
                { mk_opaque (fst $2) (of_id (snd $2)) (Some (scope_term [] (mk_pi $5 $3)))
                        (scope_term [] (mk_lam $7 $3)) }
                | KW_THM ID param+ DEF letterm DOT
                { mk_opaque (fst $2) (of_id (snd $2)) None (scope_term [] (mk_lam $5 $3)) }
                | rule+ DOT
                { mk_rules (List.map scope_rule $1) }
                | RECORD ID DEF ID LEFTBRA def_context RIGHTBRA DOT
                { mk_record_type (of_lid $2) [] $4 $6 }
                | RECORD ID param+ DEF ID LEFTBRA def_context RIGHTBRA DOT
                { mk_record_type (of_lid $2) $3 $5 $7 }
		| NEWMODULE DOT { mk_module (fst $1) (snd $1) }
		| ENDMODULE DOT { umk_module $1 }
                | command DOT { $1 }
                | EOF
                { mk_ending () ; raise Tokens.EndOfFile }


command         : WHNF  letterm    { mk_command $1 (Whnf (scope_term [] $2)) }
                | HNF   letterm    { mk_command $1 (Hnf (scope_term [] $2)) }
                | SNF   letterm    { mk_command $1 (Snf (scope_term [] $2)) }
                | STEP  letterm    { mk_command $1 (OneStep (scope_term [] $2)) }
                | INFER letterm    { mk_command $1 (Infer (scope_term [] $2)) }
                | CONV  letterm  COMMA letterm { mk_command $1 (Conv (scope_term [] $2,scope_term [] $4)) }
                | CHECK letterm  COMMA letterm { mk_command $1 (Check (scope_term [] $2,scope_term [] $4)) }
                | PRINT STRING  { mk_command $1 (Print (snd $2)) }
                | GDT   ID      { mk_command $1 (Gdt (None, of_id (snd $2))) }
                | GDT   QID     { let (_,m,v) =  of_qid $2 in mk_command $1 (Gdt (Some m,v)) }
                | OTHER term_lst { mk_command (fst $1) (Other (snd $1,List.map (scope_term []) $2)) }


term_lst        : letterm                                  { [$1] }
                | letterm COMMA term_lst                   { $1::$3 }

param           : LEFTPAR ID COLON arrterm RIGHTPAR        { PDecl (fst $2,of_id (snd $2),$4) }
                | LEFTPAR ID DEF arrterm RIGHTPAR          { PDef  (fst $2,of_id (snd $2),$4) }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW letterm
                { let (l,md_opt,id,args) = $4 in ( l , None , $2 , md_opt, id , args , $6) }
		| LEFTBRA ID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW letterm
		{ let (l,md_opt,id,args) = $7 in ( l , Some (None, of_id (snd $2)), $5 , md_opt, id , args , $9)}
		| LEFTBRA QID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW letterm
		{ let (l,md_opt,id,args) = $7 in let (_,m,v) = of_qid $2 in ( l , Some (Some m,v), $5 , md_opt, id , args , $9)}

decl            : ID COLON term         { debug 1 "Ignoring type declaration in rule context."; $1 }
                | ID                    { $1 }

def_decl        : ID COLON arrterm         { (fst $1,of_id (snd $1),$3) }

context         : /* empty */          { [] }
                | separated_nonempty_list(COMMA, decl) { $1 }

def_context     : /* empty */          { [] }
                | separated_nonempty_list(COMMA, def_decl) { $1 }

top_pattern     : ID pattern_wp*        { (fst $1,None,of_id (snd $1),$2) }
                | QID pattern_wp*       { let (l,md,id)= of_qid $1 in (l,Some md,id,$2) }


pattern_wp      : ID
                        { PPattern (fst $1,None,of_id (snd $1),[]) }
                | NUM
                        { Builtins.mk_num_patt $1 }
                | CHAR
                        { Builtins.mk_char_patt $1 }
                | STRING
                        { Builtins.mk_string_patt $1 }
                | QID
                        { let (l,md,id)= of_qid $1 in PPattern (l,Some md,id,[]) }
                | UNDERSCORE
                        { PJoker $1 }
                | LEFTBRA term RIGHTBRA
                        { PCondition $2 }
                | LEFTPAR pattern RIGHTPAR
                        { $2 }

pattern         : ID  pattern_wp+
                        { PPattern (fst $1,None,of_id (snd $1),$2) }
                | QID pattern_wp+
                        { let (l,md,id)= of_qid $1 in PPattern (l,Some md,id,$2) }
                | ID FATARROW pattern
                        { PLambda (fst $1,of_id (snd $1),$3) }
                | pattern_wp
                        { $1 }

sterm           : QID
                { let (l,md,id)=  of_qid $1 in PreQId(l,md,id) }
                | ID
                { PreId (fst $1, (of_id (snd $1))) }
                | LEFTPAR letterm RIGHTPAR
                { $2 }
                | TYPE
                { PreType $1 }
                | NUM
                { Builtins.mk_num $1 }
                | CHAR
                { Builtins.mk_char $1 }
                | STRING
                { Builtins.mk_string $1 }

term            : sterm+
                { mk_pre_from_list $1 }

letterm         : term
                { $1 }
                | ID DEF term FATARROW letterm
                { mk_let (of_id (snd $1)) $3 $5 }
                | ID COLON term ARROW letterm
                { PrePi (fst $1,Some (of_id (snd $1)),$3,$5) }
                | term ARROW letterm
                { PrePi (preterm_loc $1,None,$1,$3) }
                | ID COLON UNDERSCORE FATARROW letterm
                { PreLam (fst $1,of_id (snd $1),None,$5) }
                | ID COLON term FATARROW letterm
                { PreLam (fst $1,of_id (snd $1),Some($3),$5) }
                | ID FATARROW letterm
                { PreLam (fst $1,of_id (snd $1),None,$3) }

arrterm         : term
                { $1 }
                | ID COLON term ARROW arrterm
                { PrePi (fst $1,Some (of_id (snd $1)),$3,$5) }
                | term ARROW arrterm
                { PrePi (preterm_loc $1,None,$1,$3) }

%%
