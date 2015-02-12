val errors_in_snf : bool ref

val fail_typing_error : Typing.typing_error -> 'a
val fail_signature_error : Signature.signature_error -> 'a
val fail_env_error : Env.env_error -> 'a
val fail_dtree_error : Dtree.dtree_error -> 'a
