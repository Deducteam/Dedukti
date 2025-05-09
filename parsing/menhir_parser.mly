%{
open Kernel
open Basic
open Preterm
open Scoping
open Reduction
open Signature
open Entry

let rec mk_lam : preterm -> (loc * ident * preterm) list -> preterm = fun te ps ->
  match ps with
  | []           -> te
  | (l,x,ty)::ps -> PreLam(l, x, Some ty, mk_lam te ps)

let rec mk_pi  : preterm -> (loc * ident * preterm) list -> preterm = fun ty ps ->
  match ps with
  | []           -> ty
  | (l,x,aa)::ps -> PrePi(l, Some x, aa, mk_pi ty ps)

let mk_config loc lid =
  let strat    = ref None in
  let target   = ref None in
  let nb_steps = ref None in
  let proc = function
    | "SNF"  when !target   = None -> target   := Some Snf
    | "WHNF" when !target   = None -> target   := Some Whnf
    | "CBN"  when !strat    = None -> strat    := Some ByName
    | "CBV"  when !strat    = None -> strat    := Some ByValue
    | "CBSV" when !strat    = None -> strat    := Some ByStrongValue
    | i      when !nb_steps = None ->
       let n = int_of_string i in
       if n < 0 then raise Exit else nb_steps := Some n
    | _ -> raise Exit in
  try
    List.iter proc lid;
    { default_cfg with
      nb_steps = (!nb_steps);
      target   = (match !target with None -> default_cfg.target | Some t -> t);
      strat    = (match !strat  with None -> default_cfg.strat  | Some s -> s) }
  with _ -> raise @@ Scoping_error(loc, "invalid command configuration")

let loc_of_rs = function
  | [] -> assert false
  | (l,_,_,_,_,_,_) :: _ -> l

let make_definition ~lid ~parameters ~staticity ~is_opaque ~ty_opt ~te md =
  let loc = fst lid in
  let id = snd lid in
  let ty_opt =
    Option.map (fun ty -> scope_term md [] (mk_pi ty parameters)) ty_opt in
  let te = scope_term md [] (mk_lam te parameters) in
  Def(loc, id,  staticity, is_opaque, ty_opt, te)

let make_declaration ~lid ~parameters ~definibility ~ty md =
  let loc = fst lid in
  let id = snd lid in
  let visibility,staticity = Option.value definibility ~default:(Public, Static) in
  let ty = scope_term md [] (mk_pi ty parameters) in
  Decl(loc, id, visibility, staticity, ty)
%}

%token EOF
%token DOT
%token COMMA
%token COLON
%token EQUAL
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
%token <Kernel.Basic.loc> EVAL
%token <Kernel.Basic.loc> INFER
%token <Kernel.Basic.loc> CHECK
%token <Kernel.Basic.loc> CHECKNOT
%token <Kernel.Basic.loc> ASSERT
%token <Kernel.Basic.loc> ASSERTNOT
%token <Kernel.Basic.loc> PRINT
%token <Kernel.Basic.loc> GDT
%token <Kernel.Basic.loc> NAME
%token <Kernel.Basic.loc> REQUIRE
%token <Kernel.Basic.loc> TYPE
%token <Kernel.Basic.loc> KW_DEF
%token <Kernel.Basic.loc> KW_DEFAC
%token <Kernel.Basic.loc> KW_DEFACU
%token <Kernel.Basic.loc> KW_THM
%token <Kernel.Basic.loc> KW_INJ
%token <Kernel.Basic.loc> KW_PRV
%token <Kernel.Basic.loc*Kernel.Basic.ident> ID
%token <Kernel.Basic.loc*Kernel.Basic.mident*Basic.ident> QID
%token <string> STRING
%token <Kernel.Basic.loc*string> PRAGMA


%start line
%type <Kernel.Basic.mident -> Entry.entry> line
%type <Preterm.prule> rule
%type <Preterm.pdecl> decl
%type <Kernel.Basic.loc*Kernel.Basic.ident*Preterm.preterm> param
%type <Preterm.pdecl list> context
%type <Kernel.Basic.loc*Kernel.Basic.mident option*Kernel.Basic.ident*Preterm.prepattern list> top_pattern
%type <Preterm.prepattern> pattern
%type <Preterm.prepattern> pattern_wp
%type <Preterm.preterm> sterm
%type <Preterm.preterm> term


%%

%inline definibility:
  | KW_INJ
    { Public, Injective }
  | KW_PRV
    { Private, Definable Term.Free }
  | KW_PRV KW_INJ
    { Private, Injective }
  | KW_DEF { Public, Definable Term.Free}

let ilist(x) ==
  | l = ioption(x+) ;  { match l with | None -> [] | Some x -> x }

line:
  | rs=rule+ DOT
      {fun md -> Rules(loc_of_rs rs,(List.map (scope_rule md) rs))}
  | KW_DEF lid=ID parameters=ilist(param) ty_opt=ioption(of_ty) DEF te=term DOT
    {make_definition ~lid ~parameters ~staticity:Public ~is_opaque:false ~ty_opt ~te}
  | KW_THM lid=ID parameters=ilist(param) ty_opt=ioption(of_ty) DEF te=term DOT
    {make_definition ~lid ~parameters ~staticity:Public ~is_opaque:true ~ty_opt ~te}
  | definibility=ioption(definibility) lid=ID parameters=ilist(param) ty=of_ty DOT
    {make_declaration ~lid ~parameters ~definibility ~ty}

  | KW_DEFAC id=ID LEFTSQU ty=term RIGHTSQU DOT
    {fun md -> Decl(fst id, snd id, Public, Definable Term.AC, scope_term md [] ty)}
  | KW_PRV KW_DEFAC id=ID LEFTSQU ty=term RIGHTSQU DOT
    {fun md -> Decl(fst id, snd id, Private, Definable Term.AC, scope_term md [] ty)}
  | KW_DEFACU id=ID LEFTSQU ty=term COMMA neu=term RIGHTSQU DOT
    {fun md -> Decl(fst id, snd id, Public, Definable(Term.ACU(scope_term md [] neu)),
	            scope_term md [] ty)}
  | KW_PRV KW_DEFACU id=ID LEFTSQU ty=term COMMA neu=term RIGHTSQU DOT
    {fun md -> Decl(fst id, snd id, Private, Definable(Term.ACU(scope_term md [] neu)),
	            scope_term md [] ty)}

  | EVAL te=term DOT
      {fun md -> Eval($1, default_cfg, scope_term md [] te)}
  | EVAL cfg=eval_config te=term DOT
      {fun md -> Eval($1, cfg, scope_term md [] te)}
  | INFER te=term DOT
      {fun md -> Infer($1, default_cfg, scope_term md [] te)}
  | INFER cfg=eval_config te=term DOT
      {fun md -> Infer($1, cfg, scope_term md [] te)}

  | CHECK te=aterm COLON ty=term DOT
      {fun md -> Check($1, false, false, HasType(scope_term md [] te, scope_term md [] ty))}
  | CHECKNOT te=aterm COLON ty=term DOT
      {fun md -> Check($1, false, true , HasType(scope_term md [] te, scope_term md [] ty))}
  | ASSERT te=aterm COLON ty=term DOT
      {fun md -> Check($1, true , false, HasType(scope_term md [] te, scope_term md [] ty))}
  | ASSERTNOT te=aterm COLON ty=term DOT
      {fun md -> Check($1, true , true , HasType(scope_term md [] te, scope_term md [] ty))}

  | CHECK t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, false, false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | CHECKNOT t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, false, true , Convert(scope_term md [] t1, scope_term md [] t2))}
  | ASSERT t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, true , false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | ASSERTNOT t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, true , true , Convert(scope_term md [] t1, scope_term md [] t2))}

  | PRINT STRING DOT {fun _ -> Print($1, $2)}
  | GDT   ID     DOT {fun _ -> DTree($1, None, snd $2)}
  | GDT   QID    DOT {fun _ -> let (_,m,v) = $2 in DTree($1, Some m, v)}
  | NAME  ID    DOT {fun _ -> Name($1, mident_of_ident (snd $2))}
  | REQUIRE ID  DOT {fun _ -> Require($1, mident_of_ident (snd $2))}
  | PRAGMA {fun _ -> Pragma (fst $1, snd $1) }
  | EOF              {raise End_of_file}

eval_config:
  | LEFTSQU l=separated_nonempty_list(COMMA, ID) RIGHTSQU
  {mk_config (Lexer.loc_of_pos $startpos) (List.map (fun x -> string_of_ident (snd x)) l) }

of_ty:
  | COLON te=term { te }

param:
  | LEFTPAR id=pid COLON te=term RIGHTPAR
      {(fst id, snd id, te)}

rule:
  | LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $4 in
        ( l , None, $2 , md_opt, id , args , $6) }
  | LEFTBRA ID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $7 in
        ( l , Some (None,snd $2), $5 , md_opt, id , args , $9)}
  | LEFTBRA QID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $7 in
        let (_,m,v) = $2 in
        ( l , Some (Some m,v), $5 , md_opt, id , args , $9)}

decl:
  | ID COLON term { ($1, Some $3) }
  | ID            { ($1, None   ) }

context: separated_list(COMMA, decl) { $1 }

top_pattern:
  | ID  pattern_wp* { (fst $1,None,snd $1,$2) }
  | QID pattern_wp* { let (l,md,id)=$1 in (l,Some md,id,$2) }
  | LEFTPAR top_pattern RIGHTPAR pattern_wp* { let (loc, md, id, args) = $2 in (loc, md, id, List.append args $4) }

%inline pid:
  | ID { $1 }

pattern_wp:
  | ID                       { if snd $1 = mk_ident "_" then PJoker (fst $1,[]) else
				 PPattern (fst $1,None,snd $1,[]) }
  | QID                      { let (l,md,id)=$1 in PPattern (l,Some md,id,[]) }
  | LEFTBRA term RIGHTBRA    { PCondition $2 }
  | LEFTPAR pattern RIGHTPAR { $2 }

pattern:
  | pid FATARROW pattern     { PLambda (fst $1,snd $1,$3) }
  | pattern_wp+              { PApp($1) }

sterm:
  | QID                      { let (l,md,id)=$1 in PreQId(l,mk_name md id) }
  | ID                       { PreId (fst $1,snd $1) }
  | LEFTPAR term RIGHTPAR    { $2 }
  | TYPE                     { PreType $1 }

aterm:
  | te=sterm ts=sterm*
      {match ts with [] -> te | a::args -> PreApp(te,a,args)}

term:
  | t=aterm
      { t }
  | pid COLON aterm ARROW term
      { PrePi (fst $1,Some (snd $1), $3, $5) }
  | LEFTPAR pid COLON aterm RIGHTPAR ARROW term
      { PrePi (fst $2,Some (snd $2), $4 ,$7) }
  | aterm ARROW term
      { PrePi (Lexer.loc_of_pos $startpos,None,$1,$3) }
  | pid FATARROW term
      {PreLam (fst $1, snd $1, None, $3)}
  | pid COLON aterm FATARROW term
      {PreLam (fst $1, snd $1, Some $3, $5)}
  | LEFTPAR pid COLON aterm DEF aterm RIGHTPAR FATARROW term
      { PreApp (PreLam (fst $2, snd $2, Some $4, $9), $6, []) }
%%
