type values
type reloc_table
type to_patch_substituted
type action
type retroknowledge
type engagement = ImpredicativeSet
val val_eng : Obj.t -> unit
type polymorphic_arity = {
  poly_param_levels : Univ.universe option list;
  poly_level : Univ.universe;
}
val val_pol_arity : Obj.t -> unit
type constant_type =
    NonPolymorphicType of Term.constr
  | PolymorphicArity of Term.rel_context * polymorphic_arity
val val_cst_type : Obj.t -> unit
type substitution_domain =
    MSI of Names.mod_self_id
  | MBI of Names.mod_bound_id
  | MPI of Names.module_path
val val_subst_dom : Obj.t -> unit
module Umap :
  sig
    type key = substitution_domain
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
type resolver
type substitution = (Names.module_path * resolver option) Umap.t
type 'a subst_fun = substitution -> 'a -> 'a
val val_res : Obj.t -> unit
val val_subst : Obj.t -> unit
val fold_subst :
  (Names.mod_self_id -> 'a -> 'b -> 'b) ->
  (Names.mod_bound_id -> 'a -> 'b -> 'b) ->
  (Names.module_path -> 'a -> 'b -> 'b) -> ('a * 'c) Umap.t -> 'b -> 'b
val empty_subst : 'a Umap.t
val add_msid :
  Names.mod_self_id ->
  'a -> ('a * 'b option) Umap.t -> ('a * 'b option) Umap.t
val add_mbid :
  Names.mod_bound_id ->
  'a -> ('a * 'b option) Umap.t -> ('a * 'b option) Umap.t
val add_mp :
  Names.module_path ->
  'a -> ('a * 'b option) Umap.t -> ('a * 'b option) Umap.t
val map_msid : Names.mod_self_id -> 'a -> ('a * 'b option) Umap.t
val map_mbid : Names.mod_bound_id -> 'a -> ('a * 'b option) Umap.t
val map_mp : Names.module_path -> 'a -> ('a * 'b option) Umap.t
val subst_mp0 :
  (Names.module_path * 'a) Umap.t ->
  Names.module_path -> Names.module_path option
val subst_mp :
  (Names.module_path * 'a) Umap.t -> Names.module_path -> Names.module_path
val subst_kn0 :
  (Names.module_path * 'a) Umap.t ->
  Names.kernel_name -> Names.kernel_name option
val subst_kn :
  (Names.module_path * 'a) Umap.t -> Names.kernel_name -> Names.kernel_name
val subst_con :
  (Names.module_path * 'a) Umap.t -> Names.constant -> Names.constant
val subst_con0 :
  (Names.module_path * 'a) Umap.t -> Names.constant -> Term.constr option
val map_kn :
  (Names.mutual_inductive -> Names.mutual_inductive option) ->
  (Names.constant -> Term.constr option) -> Term.constr -> Term.constr
val subst_mps : (Names.module_path * 'a) Umap.t -> Term.constr -> Term.constr
val replace_mp_in_mp :
  Names.module_path ->
  Names.module_path -> Names.module_path -> Names.module_path
val replace_mp_in_con :
  Names.module_path ->
  Names.module_path ->
  Names.module_path * 'a * 'b -> Names.module_path * 'a * 'b
type 'a lazy_subst = LSval of 'a | LSlazy of substitution * 'a
type 'a substituted = 'a lazy_subst ref
val from_val : 'a -> 'a lazy_subst ref
val force : (substitution -> 'a -> 'a) -> 'a lazy_subst ref -> 'a
val join :
  substitution ->
  substitution -> (Names.module_path * resolver option) Umap.t
val subst_key : (Names.module_path * 'a) Umap.t -> 'b Umap.t -> 'b Umap.t
val update_subst_alias :
  ('a * 'b) Umap.t ->
  (Names.module_path * 'c) Umap.t -> ('a * 'd option) Umap.t
val join_alias :
  substitution -> substitution -> (Names.module_path * 'a option) Umap.t
val update_subst :
  ('a * 'b) Umap.t ->
  (Names.module_path * 'c) Umap.t -> ('a * 'd option) Umap.t
val subst_substituted :
  substitution -> 'a lazy_subst ref -> 'a lazy_subst ref
val force_constr : Term.constr lazy_subst ref -> Term.constr
type constr_substituted = Term.constr substituted
val val_cstr_subst : Obj.t -> unit
val subst_constr_subst :
  substitution -> 'a lazy_subst ref -> 'a lazy_subst ref
type constant_body = {
  const_hyps : Term.section_context;
  const_body : constr_substituted option;
  const_type : constant_type;
  const_body_code : to_patch_substituted;
  const_constraints : Univ.constraints;
  const_opaque : bool;
  const_inline : bool;
}
val val_cb : Obj.t -> unit
val subst_rel_declaration :
  (Names.module_path * 'a) Umap.t ->
  'b * Term.constr option * Term.constr ->
  'b * Term.constr option * Term.constr
val subst_rel_context :
  (Names.module_path * 'a) Umap.t ->
  ('b * Term.constr option * Term.constr) list ->
  ('b * Term.constr option * Term.constr) list
type recarg = Norec | Mrec of int | Imbr of Names.inductive
val val_recarg : Obj.t -> unit
val subst_recarg : (Names.module_path * 'a) Umap.t -> recarg -> recarg
type wf_paths = recarg Rtree.t
val val_wfp : Obj.t -> unit
val mk_norec : recarg Rtree.t
val mk_paths : recarg -> recarg Rtree.t list array -> recarg Rtree.t
val dest_recarg : 'a Rtree.t -> 'a
val dest_subterms : 'a Rtree.t -> 'a Rtree.t list array
val subst_wf_paths :
  (Names.module_path * 'a) Umap.t -> recarg Rtree.t -> recarg Rtree.t
type monomorphic_inductive_arity = {
  mind_user_arity : Term.constr;
  mind_sort : Term.sorts;
}
val val_mono_ind_arity : Obj.t -> unit
type inductive_arity =
    Monomorphic of monomorphic_inductive_arity
  | Polymorphic of polymorphic_arity
val val_ind_arity : Obj.t -> unit
type one_inductive_body = {
  mind_typename : Names.identifier;
  mind_arity_ctxt : Term.rel_context;
  mind_arity : inductive_arity;
  mind_consnames : Names.identifier array;
  mind_user_lc : Term.constr array;
  mind_nrealargs : int;
  mind_kelim : Term.sorts_family list;
  mind_nf_lc : Term.constr array;
  mind_consnrealdecls : int array;
  mind_recargs : wf_paths;
  mind_nb_constant : int;
  mind_nb_args : int;
  mind_reloc_tbl : reloc_table;
}
val val_one_ind : Obj.t -> unit
type mutual_inductive_body = {
  mind_packets : one_inductive_body array;
  mind_record : bool;
  mind_finite : bool;
  mind_ntypes : int;
  mind_hyps : Term.section_context;
  mind_nparams : int;
  mind_nparams_rec : int;
  mind_params_ctxt : Term.rel_context;
  mind_constraints : Univ.constraints;
  mind_equiv : Names.kernel_name option;
}
val val_ind_pack : Obj.t -> unit
val subst_const_body : substitution -> constant_body -> constant_body
val subst_arity :
  (Names.module_path * 'a) Umap.t -> inductive_arity -> inductive_arity
val subst_mind_packet :
  (Names.module_path * 'a) Umap.t -> one_inductive_body -> one_inductive_body
val subst_mind :
  (Names.module_path * 'a) Umap.t ->
  mutual_inductive_body -> mutual_inductive_body
type structure_field_body =
    SFBconst of constant_body
  | SFBmind of mutual_inductive_body
  | SFBmodule of module_body
  | SFBalias of Names.module_path * struct_expr_body option *
      Univ.constraints option
  | SFBmodtype of module_type_body
and structure_body = (Names.label * structure_field_body) list
and struct_expr_body =
    SEBident of Names.module_path
  | SEBfunctor of Names.mod_bound_id * module_type_body * struct_expr_body
  | SEBstruct of Names.mod_self_id * structure_body
  | SEBapply of struct_expr_body * struct_expr_body * Univ.constraints
  | SEBwith of struct_expr_body * with_declaration_body
and with_declaration_body =
    With_module_body of Names.identifier list * Names.module_path *
      struct_expr_body option * Univ.constraints
  | With_definition_body of Names.identifier list * constant_body
and module_body = {
  mod_expr : struct_expr_body option;
  mod_type : struct_expr_body option;
  mod_constraints : Univ.constraints;
  mod_alias : substitution;
  mod_retroknowledge : action list;
}
and module_type_body = {
  typ_expr : struct_expr_body;
  typ_strength : Names.module_path option;
  typ_alias : substitution;
}
val val_sfb : Obj.t -> unit
val val_sb : Obj.t -> unit
val val_seb : Obj.t -> unit
val val_with : Obj.t -> unit
val val_module : Obj.t -> unit
val val_modtype : Obj.t -> unit
val subst_with_body :
  substitution -> with_declaration_body -> with_declaration_body
val subst_modtype : substitution -> module_type_body -> module_type_body
val subst_structure : substitution -> structure_body -> structure_body
val subst_module : substitution -> module_body -> module_body
val subst_struct_expr : substitution -> struct_expr_body -> struct_expr_body
val subst_signature_msid :
  Names.mod_self_id -> Names.module_path -> structure_body -> structure_body
