open Basics
open Term
open Rule
open Typing

(* Wrapper around Signature *)

let sg = ref Signature.dummy

let init name = sg := Signature.make name

let get_name () = Signature.get_name !sg
let get_type l md id = Signature.get_type !sg l md id
let get_dtree l md id = Signature.get_dtree !sg l md id

let export () = Signature.export !sg

let _declare (l:loc) (id:ident) (jdg:judgment) =
  assert ( Context.is_empty jdg.ctx );
  match jdg.ty with
    | Kind | Type _ -> Signature.declare !sg l id jdg.te
    | _ -> assert false (*FIXME*)
(*         error_sort_expected jdg.te [] jdg.ty *)

let _define (l:loc) (id:ident) (jdg:judgment) =
  assert ( Context.is_empty jdg.ctx );
  Signature.define !sg l id jdg.te jdg.ty

let _define_op (l:loc) (id:ident) (jdg:judgment) =
  assert( Context.is_empty jdg.ctx );
  Signature.declare !sg l id jdg.ty

let declare l id ty =
  _declare l id (inference !sg ty)

let define l id te ty_opt =
  match ty_opt with
    | None -> _define l id (inference !sg te)
    | Some ty -> _define l id (checking !sg te ty)

let define_op l id te ty_opt =
  match ty_opt with
    | None -> _define_op l id (inference !sg te)
    | Some ty -> _define_op l id (checking !sg te ty)

let add_rules (rules: rule list) : unit =
  Signature.add_rules !sg (List.map (check_rule !sg) rules)

let infer te = (inference !sg te).ty
let check te ty = ignore (checking !sg te ty); true

let whnf te =
  let _ = inference !sg te in
    Reduction.whnf !sg te

let hnf te =
  let _ = inference !sg te in
    Reduction.hnf !sg te

let snf te =
  let _ = inference !sg te in
    Reduction.snf !sg te

let one te =
  let _ = inference !sg te in
    Reduction.one_step !sg te

let are_convertible te1 te2 =
  let _ = inference !sg te1 in
  let _ = inference !sg te2 in
    Reduction.are_convertible !sg te1 te2
