#load "type.cmo";;
#load "fastcongruence.cmo";;

open Constsigntype

open Fastcompare

let b = 0;;
(*
merge (flatten (App(Const("","f"),Const("","a"),[])), flatten (App(Const("","g"),Const("","b"),[])));
merge (flatten (App(Const("","g"),Const("","c"),[])), flatten (App(Const("","h"),(App(Const("","f"),Const("","c"),[])),[(App(Const("","g"),Const("","a"),[]))])));
merge (flatten (Const("","b")), flatten (Const("","c")));
merge (flatten (App(Const("","f"), Const("","c"),[])), flatten (App(Const("","g"),Const("","a"),[])));
merge (flatten (App(Const("","h"),Const("","d"),[Const("","d")])), flatten (App(Const("","g"),Const("","b"),[])));
merge (flatten (App(Const("","g"),Const("","a"),[])), flatten (Const("","d")));;


Table_const.bindings !representative;;
Table_sign.bindings !lookup;;
Table_const.bindings !uselist;;
Const_set.elements (classlist_value (Const1("a")));;
Const_set.elements (classlist_value (Const1("b")));;
Const_set.elements (classlist_value (Const1("c")));;
Const_set.elements (classlist_value (Const1("d")));;
Const_set.elements (classlist_value (Const1("f")));;
Const_set.elements (classlist_value (Const1("g")));;
Const_set.elements (classlist_value (Const1("h")));;
Const_set.elements (classlist_value (E(0)));;
Const_set.elements (classlist_value (E(1)));;
Const_set.elements (classlist_value (E(2)));;
Const_set.elements (classlist_value (E(3)));;
Const_set.elements (classlist_value (E(4)));;
Const_set.elements (classlist_value (E(5)));;
Const_set.elements (classlist_value (E(6)));;

areCongruent (
  (App(Const("","f"),Const("","a"),[])),
  (App(Const("","f"),Const("","c"),[]))
);;
flatten (Lam("","x",None,(App(Const("","f"),DB("","x",0),[]))));;


areCongruent (
  (App(Const("","f"),Const("","a"),[])),
  (App(Const("","f"),Const("","c"),[]))
);;

areCongruent (
  (Lam("","x",None,(App(Const("","f"),DB("","x",0),[])))),
  (Lam("","y",None,(App(Const("","f"),DB("","y",0),[])))));;
areCongruent (
  (Lam("","x",Some (App(Const("","f"),Const("","a"),[])),App(Const("","h"),(App(Const("","f"),Const("","c"),[])),[(App(Const("","g"),Const("","a"),[]))]))),
  (Lam("","x",Some (App(Const("","f"),Const("","a"),[])),App(Const("","h"),Const("","d"),[Const("","d")]))));;

areCongruent (
  (Pi("","x",(App(Const("","f"),DB("","x",0),[])),App(Const("","h"),(App(Const("","f"),Const("","c"),[])),[(App(Const("","g"),Const("","a"),[]))]))),
  (Pi("","y",(App(Const("","f"),DB("","y",0),[])),(App(Const("","h"),Const("","d"),[Const("","d")])))));;

areCongruent (
  (App(Const("","f"),Const("","a"),[])),
  (App(Const("","f"),Const("","b"),[]))
  );;
*)
(*

merge (Const("","a1"), Const("","c1"));
merge (Const("","a2"), Const("","c2"));
merge ((App(Const("","a1"),Const("","a2"),[])), (Const("","c4")));

areCongruent ((App(Const("","c1"),Const("","c2"),[])), (Const("","c4")));
Table_const.bindings !representative;;
Table_couple_const.bindings !lookup;;
*)

(* create a term which f(0) = (0,0) *)
let rec functio_u n u = let nn = n - 1 in
		    if nn == 0 then
		      App(Const("",","),u,[u])
		    else
		      App(Const("",","),(functio_u nn u),[(functio_u nn u)]);;
(*(functio_u 50 (Const("","0")));;*)
(* function_test1_init do f(u) = (App1(Const1(","),u,[u])) *)
let rec function_test1 n u=
  if n == 0 then
    u
  else
    let nn = n-1 in
    let b = constant_new (App1(Const1(","),u,[u])) in
    function_test1 nn b
;;

let rec function_test1_init n u =
  numiter := 0;
  let c = flatten (Const("",",")) in
  let b = flatten u in
  if n > 0 then
    let d = constant_new (App1(c,b,[b])) in
    function_test1 (n-1) d
  else
    function_test1 n b
;;


let b = (function_test1_init 50 (Const("","0")));;
let b2 = (function_test1_init 50 (Const("","0")));;

let c = constant_new (Clos1(b,[]));;
let d = constant_new (Clos1(b2,[]));;

let t = Sys.time();;
compare c d;;
numiter;;
Printf.printf "Execution time: %fs\n" (Sys.time() -. t);;

(* function_test2_init do f(n) = Clos(f(DB(0),DB(0)),[const(n-1)]) *)
let rec function_test2 n u app=
  if n == 0 then
    u
  else
    let nn = n-1 in
    let b = constant_new (Clos1(app,[u])) in
    function_test2 nn b app
;;

let rec function_test2_init n u =
  numiter := 0;
  let b = flatten u in
  let db = flatten (DB("","",0)) in
  let f = flatten (Const("",",")) in
  let c = constant_new (Clos1(b,[])) in
  if n > 0 then
    let app = constant_new (App1(f,db,[db])) in
    function_test2 (n-1) c app
  else
    c
;;

let f1 = (function_test2_init 50 (Const("","0")));;
let f2 = (function_test2_init 50 (Const("","0")));;

let t = Sys.time();;
compare f1 f2;;
Printf.printf "Execution time: %fs\n" (Sys.time() -. t);;
numiter;;
