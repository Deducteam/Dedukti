type token =
  | EOF
  | AT
  | DOT
  | COMMA
  | COLON
  | ARROW
  | FATARROW
  | LONGARROW
  | DEF
  | UNDERSCORE of (Types.loc)
  | NAME
  | IMPORT
  | LEFTPAR
  | RIGHTPAR
  | LEFTBRA
  | RIGHTBRA
  | LEFTSQU
  | RIGHTSQU
  | TYPE
  | ID of (Types.lvar)
  | QID of (Types.lid)

open Parsing;;
# 2 "parser.mly"
open Types
open Checker
# 29 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* AT *);
  258 (* DOT *);
  259 (* COMMA *);
  260 (* COLON *);
  261 (* ARROW *);
  262 (* FATARROW *);
  263 (* LONGARROW *);
  264 (* DEF *);
  266 (* NAME *);
  267 (* IMPORT *);
  268 (* LEFTPAR *);
  269 (* RIGHTPAR *);
  270 (* LEFTBRA *);
  271 (* RIGHTBRA *);
  272 (* LEFTSQU *);
  273 (* RIGHTSQU *);
  274 (* TYPE *);
    0|]

let yytransl_block = [|
  265 (* UNDERSCORE *);
  275 (* ID *);
  276 (* QID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\005\000\005\000\006\000\007\000\008\000\008\000\
\008\000\009\000\010\000\010\000\011\000\011\000\012\000\012\000\
\012\000\012\000\013\000\013\000\013\000\013\000\014\000\014\000\
\015\000\015\000\015\000\015\000\015\000\000\000"

let yylen = "\002\000\
\003\000\002\000\000\000\002\000\004\000\006\000\008\000\006\000\
\002\000\002\000\001\000\002\000\006\000\003\000\000\000\003\000\
\001\000\003\000\000\000\004\000\000\000\002\000\001\000\001\000\
\005\000\005\000\001\000\001\000\003\000\001\000\001\000\002\000\
\001\000\005\000\003\000\003\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\038\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\000\000\000\000\000\000\000\000\000\000\001\000\004\000\
\009\000\012\000\000\000\030\000\000\000\027\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\000\032\000\000\000\000\000\000\000\000\000\016\000\
\000\000\000\000\005\000\000\000\029\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\000\000\023\000\024\000\018\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\007\000\020\000\000\000\000\000\000\000\000\000\025\000\
\026\000"

let yydgoto = "\002\000\
\004\000\005\000\012\000\013\000\014\000\015\000\020\000\021\000\
\050\000\060\000\071\000\072\000\031\000\032\000\033\000"

let yysindex = "\009\000\
\028\255\000\000\030\255\000\000\116\255\000\000\099\255\098\255\
\107\255\112\255\111\255\113\000\116\255\135\255\122\255\054\255\
\000\000\127\255\139\255\141\255\128\255\054\255\000\000\000\000\
\000\000\000\000\054\255\000\000\081\255\000\000\000\000\070\255\
\001\255\142\255\054\255\112\255\129\255\096\255\023\255\070\255\
\054\255\000\000\000\000\054\255\054\255\054\255\144\255\000\000\
\133\255\143\255\000\000\054\255\000\000\100\255\144\255\144\255\
\006\255\042\255\054\255\080\255\054\255\052\255\054\255\054\255\
\000\000\054\255\012\255\104\255\000\000\000\000\000\000\080\255\
\144\255\000\000\144\255\144\255\134\255\133\255\133\255\133\255\
\000\000\000\000\000\000\080\255\080\255\138\255\140\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\152\000\000\000\000\000\000\000\
\000\000\137\255\000\000\000\000\152\000\000\000\153\255\000\000\
\000\000\000\000\000\000\145\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\043\255\000\000\000\000\255\254\
\000\000\000\000\000\000\137\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\255\000\000\
\064\255\000\000\000\000\000\000\000\000\000\000\018\255\062\255\
\000\000\000\000\000\000\149\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\101\255\
\027\255\000\000\078\255\094\255\000\000\109\255\121\255\121\255\
\000\000\000\000\000\000\146\255\146\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\144\000\000\000\143\000\000\000\000\000\124\000\
\000\000\245\255\224\255\000\000\227\255\121\000\234\255"

let yytablesize = 162
let yytable = "\038\000\
\033\000\033\000\043\000\033\000\039\000\044\000\033\000\065\000\
\045\000\001\000\044\000\033\000\047\000\033\000\033\000\033\000\
\044\000\014\000\055\000\036\000\036\000\056\000\057\000\058\000\
\043\000\036\000\078\000\044\000\013\000\062\000\036\000\014\000\
\036\000\036\000\036\000\053\000\067\000\003\000\073\000\081\000\
\075\000\076\000\013\000\077\000\028\000\028\000\044\000\028\000\
\006\000\066\000\028\000\086\000\087\000\074\000\028\000\028\000\
\044\000\028\000\028\000\028\000\028\000\028\000\028\000\035\000\
\035\000\027\000\083\000\084\000\085\000\035\000\019\000\028\000\
\029\000\030\000\035\000\019\000\035\000\035\000\035\000\034\000\
\034\000\027\000\019\000\019\000\040\000\034\000\041\000\028\000\
\042\000\030\000\034\000\068\000\034\000\034\000\034\000\037\000\
\037\000\051\000\069\000\070\000\044\000\037\000\016\000\052\000\
\063\000\064\000\037\000\021\000\037\000\037\000\037\000\027\000\
\023\000\021\000\022\000\019\000\017\000\028\000\042\000\030\000\
\019\000\019\000\079\000\080\000\007\000\018\000\008\000\019\000\
\019\000\009\000\019\000\010\000\019\000\019\000\011\000\082\000\
\025\000\010\000\044\000\019\000\019\000\034\000\035\000\036\000\
\037\000\046\000\059\000\049\000\044\000\061\000\088\000\003\000\
\089\000\015\000\011\000\021\000\024\000\026\000\021\000\048\000\
\054\000\017\000"

let yycheck = "\022\000\
\002\001\003\001\032\000\005\001\027\000\005\001\008\001\002\001\
\008\001\001\000\005\001\013\001\035\000\015\001\016\001\017\001\
\005\001\003\001\041\000\002\001\003\001\044\000\045\000\046\000\
\054\000\008\001\015\001\005\001\002\001\052\000\013\001\017\001\
\015\001\016\001\017\001\013\001\059\000\010\001\061\000\072\000\
\063\000\064\000\016\001\066\000\002\001\003\001\005\001\005\001\
\019\001\008\001\008\001\084\000\085\000\002\001\012\001\013\001\
\005\001\015\001\016\001\017\001\018\001\019\001\020\001\002\001\
\003\001\012\001\078\000\079\000\080\000\008\001\007\001\018\001\
\019\001\020\001\013\001\012\001\015\001\016\001\017\001\002\001\
\003\001\012\001\019\001\020\001\004\001\008\001\006\001\018\001\
\019\001\020\001\013\001\012\001\015\001\016\001\017\001\002\001\
\003\001\002\001\019\001\020\001\005\001\008\001\004\001\008\001\
\005\001\006\001\013\001\007\001\015\001\016\001\017\001\012\001\
\000\000\013\001\004\001\007\001\019\001\018\001\019\001\020\001\
\012\001\013\001\019\001\020\001\009\001\019\001\011\001\019\001\
\020\001\014\001\019\001\016\001\012\001\013\001\019\001\002\001\
\002\001\016\001\005\001\019\001\020\001\015\001\004\001\003\001\
\017\001\004\001\014\001\019\001\005\001\007\001\013\001\000\000\
\013\001\017\001\002\001\007\001\013\000\015\000\013\001\036\000\
\040\000\017\001"

let yynames_const = "\
  EOF\000\
  AT\000\
  DOT\000\
  COMMA\000\
  COLON\000\
  ARROW\000\
  FATARROW\000\
  LONGARROW\000\
  DEF\000\
  NAME\000\
  IMPORT\000\
  LEFTPAR\000\
  RIGHTPAR\000\
  LEFTBRA\000\
  RIGHTBRA\000\
  LEFTSQU\000\
  RIGHTSQU\000\
  TYPE\000\
  "

let yynames_block = "\
  UNDERSCORE\000\
  ID\000\
  QID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    Obj.repr(
# 48 "parser.mly"
                                                                ( mk_ending () )
# 203 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Types.lvar) in
    Obj.repr(
# 50 "parser.mly"
                                                                ( mk_prelude _2 )
# 210 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                                                                ( () )
# 216 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 53 "parser.mly"
                                                                ( () )
# 224 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Types.lvar) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Types.pterm) in
    Obj.repr(
# 55 "parser.mly"
                                                                ( mk_declaration (_1,_3) )
# 232 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Types.lvar) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Types.pterm) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Types.pterm) in
    Obj.repr(
# 56 "parser.mly"
                                                                ( mk_definition (_1,_3,_5) )
# 241 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : Types.lvar) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Types.pterm) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Types.pterm) in
    Obj.repr(
# 57 "parser.mly"
                                                                ( mk_opaque (_2,_5,_7) )
# 250 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Types.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Types.pterm) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Types.pterm) in
    Obj.repr(
# 58 "parser.mly"
                                                                ( mk_typecheck (_1,_3,_5) )
# 259 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Types.rule list) in
    Obj.repr(
# 59 "parser.mly"
                                                                ( mk_rules _1 )
# 266 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Types.lvar) in
    Obj.repr(
# 60 "parser.mly"
                                                                ( mk_require _2 )
# 273 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.rule) in
    Obj.repr(
# 62 "parser.mly"
                                                                ( [_1] )
# 280 "parser.ml"
               : Types.rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Types.rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Types.rule list) in
    Obj.repr(
# 63 "parser.mly"
                                                                ( _1::_2 )
# 288 "parser.ml"
               : Types.rule list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : (Types.lvar*Types.pterm) list) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Types.top_pattern) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 65 "parser.mly"
                                                                        ( (_2,_4,_6) )
# 297 "parser.ml"
               : Types.rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Types.lvar) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 67 "parser.mly"
                                                                ( (_1,_3) )
# 305 "parser.ml"
               : Types.lvar*Types.pterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                                                ( [] )
# 311 "parser.ml"
               : (Types.lvar*Types.pterm) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Types.lvar*Types.pterm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Types.lvar*Types.pterm) list) in
    Obj.repr(
# 70 "parser.mly"
                                                                ( _1::_3 )
# 319 "parser.ml"
               : (Types.lvar*Types.pterm) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.lvar*Types.pterm) in
    Obj.repr(
# 71 "parser.mly"
                                                                ( [_1] )
# 326 "parser.ml"
               : (Types.lvar*Types.pterm) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Types.lvar) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Types.pterm list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Types.pattern list) in
    Obj.repr(
# 73 "parser.mly"
                                                                ( ( (fst _1,snd _1) , Array.of_list _2 , Array.of_list _3) )
# 335 "parser.ml"
               : Types.top_pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                                                ( [] )
# 341 "parser.ml"
               : Types.pterm list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Types.pterm) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm list) in
    Obj.repr(
# 77 "parser.mly"
                                                                ( _2::_4 )
# 349 "parser.ml"
               : Types.pterm list))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                                                                ( [] )
# 355 "parser.ml"
               : Types.pattern list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Types.pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Types.pattern list) in
    Obj.repr(
# 80 "parser.mly"
                                                                ( _1::_2 )
# 363 "parser.ml"
               : Types.pattern list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.lvar) in
    Obj.repr(
# 82 "parser.mly"
                                                                ( Pat ((fst _1,!Global.name,snd _1),[||],[||]) )
# 370 "parser.ml"
               : Types.pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.lid) in
    Obj.repr(
# 83 "parser.mly"
                                                                ( Pat (_1,[||],[||]) )
# 377 "parser.ml"
               : Types.pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Types.lvar) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Types.pterm list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Types.pattern list) in
    Obj.repr(
# 84 "parser.mly"
                                                                ( Pat ((fst _2,!Global.name,snd _2),Array.of_list _3,Array.of_list _4) )
# 386 "parser.ml"
               : Types.pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Types.lid) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Types.pterm list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Types.pattern list) in
    Obj.repr(
# 85 "parser.mly"
                                                                ( Pat (_2,Array.of_list _3,Array.of_list _4) )
# 395 "parser.ml"
               : Types.pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.lid) in
    Obj.repr(
# 87 "parser.mly"
                                                                ( let (a,b,c) = _1 in PId (a,b,c) )
# 402 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.lvar) in
    Obj.repr(
# 88 "parser.mly"
                                                                ( PId (fst _1,!Global.name,snd _1) )
# 409 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Types.pterm) in
    Obj.repr(
# 89 "parser.mly"
                                                                ( _2 )
# 416 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                                                                ( PType )
# 422 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 92 "parser.mly"
                                                                ( _1 )
# 429 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Types.pterm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 93 "parser.mly"
                                                                ( PApp (_1,_2) )
# 437 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 95 "parser.mly"
                                                                ( _1 )
# 444 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Types.lvar) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Types.pterm) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 96 "parser.mly"
                                                                ( PPi  (Some _1, _3, _5) )
# 453 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Types.pterm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 97 "parser.mly"
                                                                ( PPi  (None   , _1, _3) )
# 461 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Types.lvar) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 98 "parser.mly"
                                                                ( PLam (_1, None , _3) )
# 469 "parser.ml"
               : Types.pterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Types.lvar) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Types.pterm) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Types.pterm) in
    Obj.repr(
# 99 "parser.mly"
                                                                ( PLam (_1, Some _3 , _5 ) )
# 478 "parser.ml"
               : Types.pterm))
(* Entry top *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let top (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
# 102 "parser.mly"

# 505 "parser.ml"
