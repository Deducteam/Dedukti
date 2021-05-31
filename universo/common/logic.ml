module B = Kernel.Basic
module T = Kernel.Term

module type LRA_REIFICATION =
sig
  val axiom_specification : string list * T.term
  val rule_specification  : string list * T.term
  val cumul_specification : string list * T.term
end

type z = Z
type 'a s = S

type ('a,_) op =
  | True  : 'a -> ('a,z) op
  | False : 'a -> ('a,z) op
  | Zero  : 'a -> ('a,z) op

  | Succ  : 'a -> ('a,(z s)) op

  | Minus : 'a -> ('a, (z s)) op

  | Eq    : 'a -> ('a,((z s) s)) op
  | Max   : 'a -> ('a,((z s) s)) op
  | IMax  : 'a -> ('a,((z s) s)) op
  | Le    : 'a -> ('a,((z s) s)) op

  | Ite   : 'a -> ('a,(((z s) s) s)) op

type ('a,'b) arrow =
  | Zero   : 'b -> (z, 'b) arrow
  | One    : ('b -> 'b) -> (z s, 'b) arrow
  | Two    : ('b -> 'b -> 'b) -> (((z s) s), 'b) arrow
  | Three  : ('b -> 'b -> 'b -> 'b) -> ((((z s) s) s), 'b) arrow

type ('a,'c) k =
  {
    mk: 'b. ('a,'b) op -> ('b,'c) arrow
  }

module type LRA_SPECIFICATION =
sig

  val mk_axiom  : ('c,'b) k -> 'c -> 'b -> 'b -> 'b

  val mk_rule   : ('c,'b) k -> 'c -> 'b -> 'b -> 'b  -> 'b

  val mk_cumul  : ('c,'b) k -> 'c -> 'b -> 'b -> 'b

end

module MakeLraSpecif(Lra : LRA_REIFICATION) : LRA_SPECIFICATION =
struct

  (* FIXME: exception handle *)
  (* FIXME: reify should be called once, not on every constraint *)
  let rec reify : type c b. c -> (string * b) list -> (c,b) k -> T.term -> b =
    fun ctx env k t ->
    let reify' = reify ctx env k in
    let mk_op0 (op:(_,'z) op) = match k.mk op with Zero f -> f in
    let mk_op1 op a = match k.mk op with One f -> f (reify' a) in
    let mk_op2 op a b = match k.mk op with Two f -> f (reify' a) (reify' b) in
    let mk_op3 op a b c = match k.mk op with Three f -> f (reify' a) (reify' b) (reify' c) in
    let is_cst s n = B.ident_eq (B.id n) (B.mk_ident s) in
    match t with
    | DB(_,a,_) ->
      begin
        try
          List.assoc (B.string_of_ident a) env
        with Not_found -> failwith "Wrong configuration pattern for rule"
      end
    | Const(_,n) ->
      if is_cst "true" n then
        mk_op0 (True ctx)
      else if is_cst "false" n then
        mk_op0 (False ctx)
      else if is_cst "zero" n then
        mk_op0 (Zero ctx)
      else
        failwith "Wrong configuration pattern for rule"
    | App(Const(_,n),a,[]) ->
      if is_cst "succ" n then
        mk_op1 (Succ ctx) a
      else if is_cst "minus" n then
        mk_op1 (Minus ctx) a
      else
        failwith "Wrong configuration pattern for rule"
    | App(Const(_,n),l,[r]) ->
      if is_cst "eq" n then
        mk_op2 (Eq ctx) l r
      else if is_cst "max" n then
        mk_op2 (Max ctx) l r
      else if is_cst "imax" n then
        mk_op2 (IMax ctx) l r
      else if is_cst "le" n then
        mk_op2 (Le ctx) l r
      else
        failwith "Wrong configuration pattern for rule"
    | App(Const(_,n), c,[a;b]) ->
      if is_cst "ite" n then
        mk_op3 (Ite ctx) c a b
      else
        failwith "Wrong configuration pattern for rule"
    | _ -> failwith "Wrong configuration pattern for rule"

  let mk_env2 l a' b' =
    match l with
    | [a;b] -> [(a, a'); (b, b')]
    | _ -> assert false

  let mk_env3 l a' b' c' =
    match l with
    | [a;b;c] ->
      [(a, a'); (b, b'); (c, c')]
    | _ -> assert false

  let mk_axiom k =
    let mk_axiom = ref (fun _ _ -> assert false) in
    fun ctx a b ->
      let built = ref false in
      if !built then
        !mk_axiom a b
      else
        begin
          built := true;
          let args,term = Lra.axiom_specification in
          let env = mk_env2 args a b in
          reify ctx env k term
        end

  let mk_rule k =
    let mk_rule = ref (fun _ _ _ -> assert false) in
    fun ctx a b c ->
      let built = ref false in
      if !built then
        !mk_rule a b c
      else
        begin
          built := true;
          let args,term = Lra.rule_specification in
          let env = mk_env3 args a b c in
          reify ctx env k term
        end

  let mk_cumul k =
    let mk_cumul = ref (fun _ _ -> assert false) in
    fun ctx a b ->
    let built = ref false in
    if !built then
      !mk_cumul a b
    else
      begin
        built := true;
        let args,term = Lra.cumul_specification in
        let env = mk_env2 args a b in
        reify ctx env k term
      end

end

type theory = (Universes.pred * bool) list

module type QFUF_SPECIFICATION =
sig
  val enumerate : int -> Universes.univ list
  val mk_theory : int -> theory
end
