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

let mk_char_patt (l, c) =
  PPattern (l, Some modname, _char_of_nat, [mk_num_patt_from_int l (int_of_char c)])

let rec mk_string (l, s) =
  if String.length s = 0 then
    PreQId(l, mk_name modname _string_nil)
  else
    PreApp(PreQId(l, mk_name modname _string_cons),
           mk_char (l, s.[0]),
           [mk_string (l, String.sub s 1 (String.length s - 1))])

let rec mk_string_patt (l, s) =
  if String.length s = 0 then
    PPattern (l, Some modname, _string_nil, [])
  else
    PPattern (l, Some modname, _string_cons,
              [mk_char_patt (l, s.[0]);
               mk_string_patt (l, String.sub s 1 (String.length s - 1))])

(* Exception raised when trying to print a non-atomic value *)
exception Not_atomic_builtin

let rec term_to_int = function
  | Const (_, mv)
       when mident_eq (md mv) modname &&
              ident_eq (id mv) _0 -> 0
  | App (Const (_, mv), a, [])
       when mident_eq (md mv) modname &&
              ident_eq (id mv) _S -> term_to_int a + 1
  | _ -> raise Not_atomic_builtin

let term_to_char = function
  | App (Const (_, mv), a, [])
       when mident_eq (md mv) modname &&
              ident_eq (id mv) _char_of_nat ->
     begin
       try
         char_of_int (term_to_int a)
       with Invalid_argument "char_of_int" ->
         raise Not_atomic_builtin
     end
  | _ -> raise Not_atomic_builtin

let rec term_to_string = function
  | Const (_, mv)
       when mident_eq (md mv) modname &&
              ident_eq (id mv) _string_nil -> ""
  | App (Const (_, mv), a, [b])
       when mident_eq (md mv) modname &&
              ident_eq (id mv) _string_cons ->
     Printf.sprintf "%c%s" (term_to_char a) (term_to_string b)
  | _ -> raise Not_atomic_builtin

let print_term out t =
  (* try to print the term as a numeral *)
  try
    Format.fprintf out "%d" (term_to_int t)
  with Not_atomic_builtin ->
       (* try to print as a character *)
       try
         Format.fprintf out "\'%c\'" (term_to_char t)
       with Not_atomic_builtin ->
         (* try to print as a string *)
         Format.fprintf out "\"%s\"" (term_to_string t)

let rec pattern_to_int = function
  | Pattern (_, mv, [])
       when mident_eq (md mv) modname &&
              ident_eq (id mv) _0 -> 0
  | Pattern (_, mv, [a])
       when mident_eq (md mv) modname &&
              ident_eq (id mv) _S -> pattern_to_int a + 1
  | _ -> raise Not_atomic_builtin

let print_pattern out p =
  Format.fprintf out "%d" (pattern_to_int p)
