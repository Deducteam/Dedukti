open Types 

(* Commands *)

let get_one l str = function
  | [t] -> t 
  | _   ->
      raise (MiscError (l,"Wrong number of\
                            arguments for command '#"^str^"' (Expected:1)"))

let get_two l str = function
  | [t1;t2]     -> (t1,t2)
  | _           ->
      raise (MiscError (l,"Wrong number of \
                            arguments for command '#"^str^"' (Expected:2)"))

let get_string l str = function
  | [PreId (_,p)] -> string_of_ident p
  | _             -> raise (MiscError (l,"Bad argument for command '#"^str^"'"))

let get_idents l str = function
  | [PreId (_,v)]    -> ( !Global.name , v )
  | [PreQId (_,m,v)] -> ( m , v )
  | _                ->
      raise (MiscError (l,"Bad argument for command '#"^str^"'"))

let reduce l str lst red =
  let t = fst ( Inference.infer [] (get_one l str lst) ) in
    print_string (Pp.string_of_term ( red t ) );
    print_newline ()

let exec_cmd l str lst =
  if      str="WHNF"  then reduce l str lst Reduction.wnf 
  else if str="HNF"   then reduce l str lst Reduction.hnf  
  else if str="SNF"   then reduce l str lst Reduction.snf  
  else if str="CONV"  then 
    begin
      let (pt1,pt2) = get_two l str lst in 
      let t1 = fst ( Inference.infer [] pt1 ) in
      let t2 = fst ( Inference.infer [] pt2 ) in
        if Reduction.are_convertible t1 t2 then print_string "OK\n"
        else print_string "KO\n"
    end
  else if str="CHECK" then 
    begin
      let (pte,pty) = get_two l str lst in 
      let ty1 = fst ( Inference.infer [] pty ) in
      let (te,ty2) = Inference.infer [] pte in
        if Reduction.are_convertible ty1 ty2 then print_string "OK\n"
        else print_string "KO\n"
    end
  else if str="INFER" then 
    begin
      let ty = snd ( Inference.infer [] (get_one l str lst) ) in
        print_string (Pp.string_of_term ty );
        print_newline ()
    end
  else if str="PRINT" then 
    begin
      print_string (get_string l str lst);
      print_newline ()
    end
  else if str="GDT"   then 
    begin
    let (m,v) = get_idents l str lst in 
      match Env.get_global_rw l m v with
        | None  -> Printf.fprintf stdout "No rewrite rules for '%s.%s'.\n" 
                     (string_of_ident m) (string_of_ident v)
        | Some (i,tr)   -> (
            print_string (Pp.string_of_gdt m v i tr);
            print_newline () )
    end
  else 
    raise (MiscError (l,"Unknown command '#"^str^"'"))
