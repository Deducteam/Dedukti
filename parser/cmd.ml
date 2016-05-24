open Basics
open Term
open Pp
open Typing

module type Visitor = sig
   type 'a m
    type entry
    val return         : 'a -> 'a m
    val bind           : 'a m -> ('a -> 'b m) -> 'b m
end

type command =
  (* Reduction *)
  | Whnf of term
  | Hnf of term
  | Snf of term
  | OneStep of term
  | Conv of term*term
  (*Typing*)
  | Check of term*term
  | Infer of term
  (* Misc *)
  | Gdt of ident option*ident
  | Print of string
  | Other of string*term list


module type C = sig
  type 'a m
  type entry
  val mk_command : loc -> command -> entry m
  val print_command : Format.formatter -> command -> unit
end


let print s = Format.printf "%s\n" s

module Make(M:Visitor) = struct 

  type 'a m = 'a M.m
  type entry = Default.signature

let mk_command lc = function
  | Whnf te          ->
      ( match Env.whnf te with
          | OK te -> M.return [(Default.Term te)] (* Format.printf "%a\n" print_term te *)
          | Err e -> Errors.fail_env_error e )
  | Hnf te           ->
      ( match Env.hnf te with
          | OK te -> M.return [(Default.Term te)] (* Format.printf "%a\n" print_term te *)
          | Err e -> Errors.fail_env_error e )
  | Snf te           ->
      ( match Env.snf te with
          | OK te -> M.return [(Default.Term te)] (* Format.printf "%a\n" print_term te *)
          | Err e -> Errors.fail_env_error e )
  | OneStep te       ->
      ( match Env.one te with
          | OK (Some te) -> M.return [(Default.Term te)] (* Format.printf "%a\n" print_term te *)
          | OK None -> M.return [(Default.Term te)] (* Format.printf "%a\n" print_term te *)
          | Err e -> Errors.fail_env_error e )
  | Conv (te1,te2)  ->
        ( match Env.are_convertible te1 te2 with
            | OK true -> M.return [(Default.String "YES")] (* print "YES" *)
            | OK false -> M.return [(Default.String "NO")] (* print "NO" *)
            | Err e -> Errors.fail_env_error e )
  | Check (te,ty) ->
        ( match Env.check te ty with
            | OK () -> M.return [(Default.String "YES")] (* print "YES" *)
            | Err e -> Errors.fail_env_error e )
  | Infer te         ->
      ( match Env.infer te with
          | OK ty -> M.return [(Default.Term ty)] (* Format.printf "%a\n" print_term ty *)
          | Err e -> Errors.fail_env_error e )
  | Gdt (m0,v)         ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
        ( match Env.get_dtree lc m v with
            | OK (Some (i,g)) -> M.return [(Default.String "TODO")]	      
                (* Printf.fprintf stdout "%a\n" Rule.pp_rw (m,v,i,g) *)
            | _ -> M.return [(Default.String "No GDT")] (* print "No GDT." *) )
  | Print str         -> M.return [(Default.String str)] (* Format.printf "%s" str *)
  | Other (cmd,_)     -> M.return [(Default.String ("Unknow command '"^cmd^"'.\n"))] (* prerr_string ("Unknown command '"^cmd^"'.\n") *)

let print_command out c =
  match c with
  | Whnf te          -> Format.fprintf out "#WHNF@ %a." print_term te
  | Hnf te           -> Format.fprintf out "#HNF@ %a." print_term te
  | Snf te           -> Format.fprintf out "#SNF@ %a." print_term te
  | OneStep te       -> Format.fprintf out "#STEP@ %a." print_term te
  | Conv (te1,te2)   -> Format.fprintf out "#CONV@ %a@ %a." print_term te1 print_term te2
  | Check (te,ty)    -> Format.fprintf out "#CHECK@ %a@ %a." print_term te print_term ty
  | Infer te         -> Format.fprintf out "#INFER@ %a." print_term te
  | Gdt (m0,v)       ->
      begin match m0 with
      | None -> Format.fprintf out "#GDT@ %a." print_ident v
      | Some m -> Format.fprintf out "#GDT@ %a.%a." print_ident m print_ident v
      end
  | Print str         -> Format.fprintf out "#PRINT \"%s\"." str
  | Other (cmd,_)     -> failwith ("Unknown command '"^cmd^"'.\n")

end
