open Types
open Matrix

(* Construct a decision tree out of a matrix *)
let rec to_dtree (mx:matrix) : dtree =
  match choose_column mx with
    (* Only variables on the first line of the matrix *)
    | None   ->
        let line = first mx in
          Test ( (*line.l_esize,*) line.l_ctx, line.l_eqs, line.l_rhs,
                 map_opt to_dtree (pop mx) )
    (* Pattern on the first line at column c *)
    | Some c ->
        let cases = partition mx c in
        let aux ca = ( ca , to_dtree (specialize mx c ca) ) in
          Switch (c, List.map aux cases, map_opt to_dtree (default mx c) )

let add_rules (rwi:rw_infos) (rs:rule list) : rw_infos =
  assert (rs != [] ) ;
  let ( ty , rules ) = match rwi with
    | Decl ty                   -> ( ty , rs )
    | Decl_rw (ty,rs0,_,_)       -> ( ty , rs0@rs )
    | Def (_,_)                 ->
        let r = match rs with r::_ -> r | _ -> assert false in
          Global.fail r.l "Cannot add rewrite\
            rules for the defined symbol '%a'." pp_ident r.id
  in
  let mx = mk_matrix rules in
    Decl_rw ( ty, rules , width mx , to_dtree mx )
