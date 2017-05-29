let print_delta_trace = ref true

let print_beta_trace = ref false

type context =
  { term : Term.term;
    ty : Term.term option;
    var : Basic.ident;
    ctx : (Basic.ident * Term.term) list
  }

let print_context out ctx =
  begin
    match ctx.ty with
    | None ->
      Format.fprintf out "Hole: %a@." Pp.print_ident ctx.var
    | Some ty ->
      Format.fprintf out "Hole: %a : %a@." Pp.print_ident ctx.var Pp.print_term ty
  end;
  Format.fprintf out "Term: %a@." Pp.print_term ctx.term


let fresh_ctx_var =
  let (c:int ref) = ref (-1) in
  let prefix = "ctx_var" in
  fun () -> incr c; Basic.hstring @@ prefix^(string_of_int !c)

let closure ctx =
  Term.mk_Lam Basic.dloc ctx.var ctx.ty ctx.term

let compose ctx ctx' =
  let rec plug_term ctx_term t =
    match ctx_term with
    | Term.DB(_, id,_) when id = ctx.var -> t
    | Term.Lam(l,id,Some ty, te) ->
      Term.mk_Lam l id  (Some(plug_term ty t)) (plug_term te t)
    | Term.App(f,a,args) ->
      Term.mk_App (plug_term f t) (plug_term a t) (List.map (fun x -> plug_term x t) args)
    | Term.Pi(l,id, ty, te) ->
      Term.mk_Pi l id (plug_term ty t) (plug_term te t)
    | _ -> ctx_term
  in
  let term = plug_term ctx.term ctx'.term in
  { ctx' with term; ctx=ctx'.ctx@ctx.ctx}

let apply_ctx ctx term =
  Term.mk_App (closure ctx) term []

exception Mismatch of context * Term.term

let get_hole ctx term =
  let rec get_hole ctxt term =
    match ctxt,term with
    | Term.Kind, Term.Kind -> None
    | Term.Type _, Term.Type _ -> None
    | Term.DB(l,id,n), Term.DB(l',id',n') ->
      if n = n' then None else raise (Mismatch(ctx,term))
    | Term.App(f,a,args), Term.App(f',a',args') when List.length (a::args) = List.length (a'::args') ->
      begin
        match get_hole f f' with
        | None ->
          List.fold_left2
            (fun r left right ->
               match r with
               | None -> get_hole left right
               | Some x ->
                 if left = right then
                   Some x
                 else raise (Mismatch(ctx,term))) None (a::args) (a'::args')
        | Some x ->
          if a = a' && args = args' then
            Some x
          else
            raise (Mismatch(ctx,term))
      end
    | Term.App(Term.DB(_,id,_), a, args), Term.App(f,a',args') ->
      Some (Term.mk_App f a' [])
    | Term.Lam(l,id,pty, te), Term.Lam(l',id',pty',te') ->
      if pty = pty' then
        get_hole te te'
      else
        raise (Mismatch(ctx,term))
    | Term.Pi(l,id,ty, te), Term.Pi(l',id',ty',te') ->
      if ty = ty' then
        get_hole te te'
      else
        raise (Mismatch(ctx,term))
    | Term.DB(_,id,_),_ when id = ctx.var -> Some term
    | Term.Const(l,md,id), Term.Const(l',md',id') ->
      if md = md' && id = id' then None else raise (Mismatch(ctx,term))
    | _,_ -> Format.printf "left: %a@.right: %a@." Pp.print_term ctxt Pp.print_term term; raise (Mismatch(ctx,term))
  in
  match get_hole ctx.term term with
  | None -> raise (Mismatch(ctx,term))
  | Some x -> x


let compare_terms left right : context option =
  let make_ctx =
    let b = ref false in
    fun k ctx -> if !b = true then assert false else b:= true;
      let var = fresh_ctx_var () in
      {term = Term.mk_DB Basic.dloc var k; ty = None; var; ctx}
  in
  let rec ctx_of_list f a comp (st:Term.term list) (args:Term.term list) args' =
    match args,args' with
    | [],_
    | _, [] -> assert false (* contradict args <> args' *)
    | x::t, x'::t' ->
      begin
        match comp x x' with
        | None -> ctx_of_list f a comp (st@[x]) t t'
        | Some ctx -> { ctx with term = Term.mk_App f a (st@[ctx.term]@t)}
      end
  in
  let rec compare_terms k ctx left right =
    if Term.term_eq left right then
      None
    else
      match left,right with
      | Term.Kind, Term.Kind -> None
      | Term.Type _, Term.Type _ -> None
      | Term.App(Term.Lam(_,id,_,_),_,[]), Term.App(Term.Lam(_,id',_,_),_,[]) when id <> id' -> Some (make_ctx k ctx)
      (* FIXME: the case below is too large. *)
      | Term.App(Term.Lam(_,id,Some ty,te)as lam,a,[]), Term.App(Term.Lam(_,id', Some ty', te') as lam',a',[]) when id = id' ->
        if lam = lam' then
          match compare_terms k ctx a a' with
          | None -> assert false
          | Some x -> Some {x with term = (Term.mk_App lam x.term [])}
        else
          if ty = ty' && a = a' then
            match compare_terms (k+1) ((id,ty)::ctx) te te' with
            | None -> assert false
            | Some x -> Some {x with term = (Term.mk_App (Term.mk_Lam Basic.dloc id (Some ty) x.term) a [])}
          else
            Some (make_ctx k ctx)
      | Term.App(Term.Lam(_,id,_,_),_,[]), _ -> Some (make_ctx k ctx)
      (*One case of beta redex, the other is the last case. : x = a' *)
      | Term.App(Term.Lam(_,_,_,_),a,x::t), Term.App(_,a',args) when List.length (x::t) <> List.length args ->
        let ctx = make_ctx k ctx in
        Some {ctx with term= Term.mk_App ctx.term x t}
      | Term.App(f,a,args), Term.App(f',a',args') ->
      begin
        match compare_terms k ctx f f' with
        | None ->
          begin
            match compare_terms k ctx a a' with
            | None ->
              if List.length args <> List.length args' then
                Some (make_ctx k ctx)
              else
                Some (ctx_of_list f a (compare_terms k ctx) [] args args')
            | Some x ->
              if args = args' then
                Some {x with term = (Term.mk_App f x.term args)}
              else
                Some (make_ctx k ctx)
          end
        | Some x ->
          if a = a' && args = args' then
            Some {x with term = (Term.mk_App x.term a args)}
          else
            Some (make_ctx k ctx)
      end
      | Term.Lam(_,_,None, _), _
      | _,Term.Lam(_,_,None, _) -> failwith "every lambda should be typed"
      | Term.Lam(l,id,Some pty, te), Term.Lam(l',id',Some pty',te') ->
        if pty = pty' then
          match compare_terms (k+1) ((id,pty)::ctx) te te' with
          | None -> assert false
          | Some ctx -> Some {ctx with term=Term.mk_Lam l id (Some pty)  ctx.term}
        else
          Some (make_ctx k ctx)
      | Term.Pi(l,id,ty, te), Term.Pi(l',id',ty',te') ->
        if ty = ty' then
          match compare_terms k ctx te te' with
          | None -> assert false
          | Some ctx -> Some {ctx with term=Term.mk_Pi l id ty ctx.term}
        else
          Some (make_ctx k ctx)
      | _,_ -> Some (make_ctx k ctx)
  in
  compare_terms 0 [] left right

type trace_step =
  {
    ctx: context;
    rule:Rule.rule_name;
  }

let print_trace out step =
  Format.printf "Rule: %a@." Rule.pp_rule_name step.rule;
  Format.printf "Context: %a@." print_context step.ctx


type trace = trace_step list

let only_delta : Reduction.red =
  {
    Reduction.beta = false;
    Reduction.select = Some
        (fun r ->
           match r with
           | Rule.Delta(md,id) when md = Basic.hstring "hol" -> false
           | Rule.Delta _ -> true
           | _ -> false)
  }

let delta_step term =
  Env.unsafe_one_step ~red:only_delta term


let only_beta : Reduction.red =
  {
    Reduction.beta = true;
    Reduction.select = Some (fun _ -> false)
  }

let beta_step term =  Env.unsafe_one_step ~red:only_beta term

let rec snf_delta term =
  let term' = delta_step term in
  if Term.term_eq term term' then
      term
  else
    snf_delta term'

let rec snf_beta term =
  let term' = beta_step term in
  if Term.term_eq term term' then
    term
  else
    snf_beta term'

let beta_step_to_trace term =
  let term' = beta_step term in
  match compare_terms term term' with
  | None -> None
  | Some ctx ->
    match  get_hole ctx term with
    | Term.App(Term.Lam(_,id,Some ty, te),a,[]) as redex ->
      let rule = Rule.Delta(Basic.hstring "hol", Basic.hstring"beta") in
      Some(term', {ctx;rule}, redex)
    | _ -> assert false





let step_to_trace term =
  let term' = delta_step term in
  match compare_terms term term' with
  | None -> None
  | Some ctx ->
    let ty,rule =
      match get_hole ctx term with
      | Term.Const(dloc,md,id) ->
        let ty =
          begin
            match Env.get_type dloc md id with
            | Basic.OK ty -> ty
            | Basic.Err er -> Errors.fail_signature_error er
          end
        in
        ty, Rule.Delta(md,id)
      | t -> Errors.fail Basic.dloc "Should be a constant %a.@.%a@." Pp.print_term t Pp.print_term ctx.term
    in
    Some(term',{ctx={ctx with ty = Some ty};rule})

let rec trace term =
  match step_to_trace term with
  | None -> term,[]
  | Some(term',ctx) ->
    let t,l = trace term' in
    t,ctx::l

let rec beta_trace term =
  match beta_step_to_trace term with
  | None -> term,[]
  | Some(term',ctx,redex) ->
    let t,l = beta_trace term' in
    t, (ctx,redex)::l

type direction = Fold | Unfold


let prefix_eq = "__eq__"

let prefix_sym_eq = prefix_eq^"sym_"

let print_equality id ty t =
  let name = prefix_eq^(Basic.string_of_ident id) in
  Format.printf "@[<2>%s : hol.eps (hol.leibniz (%a) %a (%a)).@]@.@." name Pp.print_term ty Pp.print_ident id Pp.print_term t;
  let name_sym = prefix_sym_eq^(Basic.string_of_ident id) in
  Format.printf "@[<2>%s : hol.eps (hol.leibniz (%a) (%a) %a).@]@.@." name_sym Pp.print_term ty Pp.print_term t Pp.print_ident id

let const_of_rule_name dir name =
  match name with
  | Rule.Delta(md,id) ->
    let prefix = match dir with Unfold -> prefix_eq | Fold -> prefix_eq in
    let id' = Basic.hstring (prefix^(Basic.string_of_ident id)) in
    Term.mk_Const Basic.dloc md id'
  | _ -> failwith "not handled right now"


let leibnize_step dir step term =
  Term.mk_App (const_of_rule_name dir step.rule) (closure step.ctx) [term]

let leibnize_beta_step dir step term redex =
  let rec closure_ctx l =
    match l with
    | [] -> redex
    | (id,ty)::t -> Term.mk_Lam Basic.dloc id (Some ty) (closure_ctx t)
  in
  Term.mk_App (const_of_rule_name dir step.rule) (closure_ctx @@ List.rev step.ctx.ctx)  [(closure step.ctx);term]


let leibnize dir term ty =
  let context_of_proof ty =
    match ty with
    | Term.App(_,ty,_) -> ty
    | _ -> Format.printf "Type: %a@." Pp.print_term ty; assert false
  in
  let ty' = context_of_proof ty in
  try
    let ty'',tr = if !print_delta_trace then trace ty' else ty', [] in
    let _,btr = if !print_beta_trace then beta_trace ty'' else ty'', [] in
    let term' = List.fold_left
        (fun term (step,redex) -> leibnize_beta_step Unfold step term redex) term (List.rev btr) in
    (* Format.printf "type: %a@.length: %d@." Pp.print_term ty' (List.length tr); *)
    match dir with
    | Fold -> List.fold_right (fun step te -> leibnize_step dir step te) tr term'
    | Unfold -> List.fold_left (fun te step -> leibnize_step dir step te) term' tr
  with Mismatch(ctx,term) -> Errors.fail Basic.dloc "should not happen, mismatch between ctx and term:@.%a@.%a@." print_context ctx Term.pp_term term



let is_cst ty =
  match ty with
  | Term.App (cst,_,_) ->
    begin
      match cst with
      | Term.Const(_,md,id) -> Basic.string_of_ident id = "eta" && Basic.string_of_ident md = "hol"
      | _ -> false
    end
  | _ -> false

let is_proof ty =
  match ty with
  | Term.App (cst,_,_) ->
    begin
      match cst with
      | Term.Const(_,md,id) -> Basic.string_of_ident id = "eps" && Basic.string_of_ident md = "hol"
      | _ -> false
    end
  | _ -> false

let cst_proof = Term.mk_Const Basic.dloc (Basic.hstring "proof") (Basic.hstring "proof")
let cst_term = Term.mk_Const Basic.dloc (Basic.hstring "term") (Basic.hstring "term")

let leibnize_term term =
  let is_forall ty =
    match ty with
    | Term.App(Term.Const(_,md,id),_,_) ->
      Basic.string_of_ident md = "hol" && Basic.string_of_ident id = "forall"
    | _ -> false
  in
  let rec leibnize_term term =
    match term with
    | Term.Kind
    | Term.Type _
    | Term.DB _ -> term
    | Term.Const(l,md,id) when Basic.string_of_ident md = "hol" -> term
    | Term.Const(l,md,id) when Basic.string_of_ident md = "term" || Basic.string_of_ident md = "proof" -> term
    | Term.Const(l,md,id) (* md <> "hol" *) ->
      begin
        match Env.get_type l md id with
        | Basic.OK ty ->
          if is_proof ty then
            leibnize Unfold term ty
          else
            (snf_delta term)
        | Basic.Err er -> Errors.fail_signature_error er
      end;
    | Term.App(Term.Const(l,md,id),a,args) ->
      let fold t arg =
        if is_forall t then
          let t' = Env.unsafe_one_step t in
          match t' with
          | Term.Pi(_,id,a,b) ->
            let b' = Subst.subst b arg in
            let t'' = Env.unsafe_one_step b' in
            Format.printf "debug: %a@.@." Term.pp_term t'';
            beta_step t''
          | _ -> Format.printf "type: %a@." Term.pp_term t'; assert false
        else
          let t' = Env.unsafe_one_step t in
          match t' with
          | Term.Pi(_,id,a,b) ->
            let b' = Subst.subst b arg in
            let t'' = Env.unsafe_one_step b' in
            Format.printf "debug: %a@.@." Term.pp_term t'';
            t''
          | _ -> Format.printf "type: %a@." Term.pp_term t'; assert false
      in
      begin
        match Env.get_type l md id with
        | Basic.OK ty ->
          List.fold_left fold ty (a::args)
        | Basic.Err er -> Errors.fail_signature_error er
      end
    | Term.App(f,a,args) ->
      Term.mk_App (leibnize_term f) (leibnize_term a) (List.map leibnize_term args)
    | Term.Lam(l,id, Some ty, te) ->
      Term.mk_Lam l id (Some (snf_delta ty)) (leibnize_term te)
    | Term.Lam(l,id, None, te) ->
      Term.mk_Lam l id None (leibnize_term te)
    | Term.Pi(l,id, ty, te) ->
      Term.mk_Pi l id ty (leibnize Unfold te ty)
  in
  leibnize_term term
