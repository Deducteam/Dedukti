module Check = struct
  module Ok = struct
    let ok ~basename =
      let filename = "tests/OK/" ^ basename in
      Dedukti.Check.ok ~filename

    let _ =
      ok ~basename:"*1.dk" [Export];
      ok ~basename:"*2.dk" [Import "tests/OK"];

      ok ~basename:"~^+ ∉a.dk" [Export];
      ok ~basename:"~^+ ∉b.dk" [Import "tests/OK"];

      ok ~basename:"dep_A.dk" [Export];
      ok ~basename:"dep_B.dk" [Import "tests/OK"];
      ok ~basename:"dep C.dk" [Export];
      ok ~basename:"dep D.dk" [Import "tests/OK"];

      ok ~basename:"SR_sat_2.dk" [Type_lhs];
      ok ~basename:"SR_sat_bv1.dk" [Type_lhs];
      ok ~basename:"SR_sat_bv2.dk" [Type_lhs];
      ok ~basename:"SR_sat_eq1.dk" [Type_lhs];
      ok ~basename:"SR_sat_eq2.dk" [Type_lhs];

      ok ~basename:"SR_OK_1.dk" [Sr_check 1];
      ok ~basename:"SR_OK_2.dk" [Sr_check 2];
      ok ~basename:"SR_OK_3.dk" [Sr_check 3];
      ok ~basename:"SR_OK_4.dk" [Sr_check 4];

      ok ~basename:"brackets0.dk" [];
      ok ~basename:"brackets0b.dk" [];
      ok ~basename:"brackets1.dk" [];
      ok ~basename:"brackets2.dk" [];
      ok ~basename:"brackets3.dk" [];
      ok ~basename:"brackets4.dk" [];
      ok ~basename:"brackets5.dk" [];

      ok ~basename:"cstr_ignored1.dk" [];
      ok ~basename:"cstr_ignored2.dk" [];
      ok ~basename:"cstr_ignored3.dk" [];

      ok ~basename:"arities.dk" [];
      ok ~basename:"arities2.dk" [];

      ok ~basename:"first_order_cstr1.dk" [];
      ok ~basename:"firstOrder_v2.dk" [];

      ok ~basename:"guard1.dk" [];
      ok ~basename:"guard2.dk" [];
      ok ~basename:"guard3.dk" [];

      ok ~basename:"higher_order_cstr1.dk" [];
      ok ~basename:"higher_order_cstr2.dk" [];
      ok ~basename:"higher_order_cstr3.dk" [];

      ok ~basename:"ho_bug1.dk" [];
      ok ~regression:true ~basename:"ho_bug2.dk" [];

      ok ~regression:true ~basename:"miller1.dk" [];
      ok ~regression:true ~basename:"miller2.dk" [];

      ok ~basename:"nonlinearity.dk" [];

      ok ~regression:true ~basename:"nsteps1.dk" [];
      ok ~regression:true ~basename:"nsteps2.dk" [];
      ok ~regression:true ~basename:"nsteps3.dk" [];

      ok ~basename:"rule_name2.dk" [];

      ok ~basename:"self_dep.dk" [];
      ok ~basename:"self_dep2.dk" [];
      ok ~basename:"self_dep3.dk" [];

      ok ~regression:true ~basename:"special_idents1.dk" [Export];
      ok ~basename:"special_idents2.dk" [Import "tests/OK"];
      ok ~basename:"special_idents3.dk" [];

      ok ~basename:"type_annot_cstr.dk" [];
      ok ~basename:"type_annot_cstr2.dk" [];
      ok ~basename:"type_annot_cstr3.dk" [];

      ok ~basename:"typable_lhs.dk" [];
      ok ~basename:"typable_lhs2.dk" [];

      ok ~basename:"underscore1.dk" [];
      ok ~basename:"underscore2.dk" [];
      ok ~basename:"underscore3.dk" [];
      ok ~basename:"underscore4.dk" [];
      ok ~basename:"underscore5.dk" [];
      ok ~basename:"underscore6.dk" [];
      ok ~basename:"underscore7.dk" [];

      ok ~basename:"domainfree.dk" [];
      ok ~basename:"pattern_parentheses_issue259.dk" [];
      ok ~basename:"firstOrder.dk" [];
      ok ~basename:"recursive.dk" [];
      ok ~regression:true ~basename:"sharing.dk" [];
      ok ~regression:true ~basename:"inferingKindForType.dk" [];
      ok ~basename:"ho_match.dk" [];
      ok ~basename:"tptp.dk" [];
      ok ~regression:true ~basename:"inferingKindForArrowWithCodomainType.dk" [];
      ok ~basename:"doubleneg.dk" [];
      ok ~basename:"WIP.dk" [];
      ok ~basename:"ho_nonlinearity.dk" [];
      ok ~basename:"subst.dk" [];
      ok ~basename:"rule_name.dk" [];
      ok ~basename:"let_syntax.dk" [];
      ok ~basename:"type_annot_readme.dk" [];
      ok ~basename:"injective_smb.dk" [];
      ok ~basename:"hott.dk" [];
      ok ~basename:"nested_comments.dk" [];
      ok ~regression:true ~basename:"fixpoints.dk" [];
      ok ~basename:"type_annot.dk" [];
      ok ~basename:"rule_order.dk" [];
      ok ~regression:true ~basename:"nested_miller_pattern.dk" [];
      ok ~regression:true ~basename:"nsteps4.dk" [];
      ok ~basename:"dotpat.dk" [];
      ok ~basename:"type_rewrite.dk" []
  end
end

let _ =
  Dedukti.Check.ok ~filename:"tests/eta/OK/eta_0.dk" [Eta];
  Dedukti.Check.ko ~error:(`Code 704) ~filename:"tests/eta/KO/eta_0.dk" [];
  Dedukti.Meta.run ~filename:"tests/meta/simple.dk" [];
  Dedukti.Meta.run ~filename:"tests/meta/simple.dk" [No_meta];
  Dedukti.Meta.run ~filename:"tests/meta/beta.dk" [];
  Dedukti.Meta.run ~filename:"tests/meta/beta.dk" [No_beta];
  Dedukti.Meta.run ~filename:"tests/meta/beta.dk" [No_beta; No_meta];
  Dedukti.Meta.run ~dep:["tests/meta/simple_2.dk"]
    ~filename:"tests/meta/simple_2.dk"
    [Meta "tests/meta_files/meta.dk"];
  Dedukti.Meta.run ~dep:["tests/meta/simple_2.dk"]
    ~filename:"tests/meta/simple_2.dk"
    [Meta "tests/meta_files/meta.dk"; Meta "tests/meta_files/meta2.dk"];
  Dedukti.Meta.run
    ~dep:["tests/meta/rewrite_prod.dk"]
    ~check_output:false ~filename:"tests/meta/rewrite_prod.dk"
    [Meta "tests/meta_files/prod_meta.dk"; Quoting `Prod; No_unquoting];
  Dedukti.Meta.run
    ~dep:["tests/meta/rewrite_prod.dk"]
    ~filename:"tests/meta/rewrite_prod.dk"
    [Meta "tests/meta_files/prod_meta.dk"; Quoting `Prod];
  Dedukti.Pretty.run ~filename:"tests/OK/hott.dk" [];
  Dedukti.Universo.run ~filename:"tests/universo/simple_ok.dk"
    [
      Config "tests/universo/config/universo_cfg.dk";
      Theory "tests/universo/theory/cts.dk";
      Import "tests/universo/theory";
      Output_directory "tests/universo/output";
    ];
  (* TODO: fix this one *)
  (* Dedukti.Universo.run ~filename:"tests/universo/simple_ok.dk"
   *   [
   *     Config "tests/universo/config/universo_cfg.dk";
   *     Theory "tests/universo/theory/cts.dk";
   *     Import "tests/universo/theory";
   *     Output_directory "tests/universo/output";
   *     Simplify "tests/universo/simplified_output";
   *   ]; *)
  (* TODO: fix this one too *)
  (* Dedukti.Universo.run ~fails:true ~filename:"tests/universo/simple_ko.dk"
   *   [
   *     Config "tests/universo/config/universo_cfg.dk";
   *     Theory "tests/universo/theory/cts.dk";
   *     Import "tests/universo/theory";
   *     Output_directory "tests/universo/output";
   *   ]; *)
  Test.run ()
