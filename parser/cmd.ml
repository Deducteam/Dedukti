open Term

type command =
  (* Reduction *)
  | Whnf of preterm
  | Hnf of preterm
  | Snf of preterm
  | OneStep of preterm
  | Conv of preterm*preterm
  (*Typing*)
  | Check of preterm*preterm
  | Infer of preterm
  (* Misc *)
  | Gdt of ident option*ident
  | Print of string
  | Other of string*preterm list

let print s= print_string s; print_newline ()

let mk_command lc = function
  | Whnf pte          ->
      let (te,_) = Inference.infer2 pte in
        Pp.pp_term stdout (Reduction.whnf te)
  | Hnf pte           ->
      let (te,_) = Inference.infer2 pte in
        Pp.pp_term stdout (Reduction.hnf te)
  | Snf pte           ->
      let (te,_) = Inference.infer2 pte in
        Pp.pp_term stdout (Reduction.snf te)
  | OneStep pte       ->
      let (te,_) = Inference.infer2 pte in
        ( match Reduction.one_step te with
            | None    -> print "Already in weak head normal form."
            | Some t' -> Pp.pp_term stdout t')
  | Conv (pte1,pte2)  ->
      let (t1,_) = Inference.infer2 pte1 in
      let (t2,_) = Inference.infer2 pte2 in
        if Reduction.are_convertible t1 t2 then print "OK"
        else print "KO"
  | Check (pte,pty) ->
      let (ty1,_) = Inference.infer2 pty in
      let (_,ty2) = Inference.infer2 pte in
        if Reduction.are_convertible ty1 ty2 then print "OK"
        else print "KO"
  | Infer pte         ->
      let (te,ty) = Inference.infer2 pte in Pp.pp_term stdout ty
  | Gdt (m0,v)         ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
        ( match Env.get_infos lc m v with
            | Env.Decl_rw (_,_,i,g) -> Pp.pp_rw stdout (m,v,i,g)
            | _                 -> print "No GDT." )
  | Print str         -> output_string stdout str
  | Other (cmd,_)     -> prerr_string ("Unknown command '"^cmd^"'.\n")
