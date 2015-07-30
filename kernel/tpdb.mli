open Rule

val init : out_channel -> unit
val pp_pattern : out_channel -> pattern -> unit
val pp_rule : out_channel -> rule_infos -> unit
