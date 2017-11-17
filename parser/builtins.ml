open Basic
open Preterm
open Term
open Printf
open Rule

let modname = mk_mident "builtins"

(* Constructors *)
let _0 = mk_ident "0"
let _S = mk_ident "S"
let _char_of_nat = mk_ident "char_of_nat"
let _string_nil = mk_ident "string_nil"
let _string_cons = mk_ident "string_cons"
let _nil = mk_ident "nil"
let _cons = mk_ident "cons"

let rec mk_num_from_int l = function
    | 0 -> PreQId(l, mk_name modname _0)
    | n -> PreApp(PreQId(l, mk_name modname _S), mk_num_from_int l (n - 1), [])

let mk_num (l, s) = mk_num_from_int l (int_of_string s)

let rec mk_num_patt_from_int l = function
  | 0 -> PPattern (l, Some modname, _0, [])
  | n -> PPattern (l, Some modname, _S, [mk_num_patt_from_int l (n-1)])

let mk_num_patt (l, s) = mk_num_patt_from_int l (int_of_string s)

let mk_char (l, c) =
  PreApp(PreQId(l, mk_name modname _char_of_nat), mk_num_from_int l (int_of_char c), [])

let rec mk_string (l, s) =
  if String.length s = 0 then
    PreQId(l, mk_name modname _string_nil)
  else
    PreApp(PreQId(l, mk_name modname _string_cons), mk_char (l, s.[0]), [mk_string (l, String.sub s 1 (String.length s - 1))])
