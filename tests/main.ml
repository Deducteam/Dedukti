let _ =
  Dedukti.Check.ok ~filename:"tests/OK/recursive.dk" [];
  Dedukti.Check.ok ~filename:"tests/eta/OK/eta_0.dk" [Eta];
  Dedukti.Check.ko ~error:(`Code 704) ~filename:"tests/eta/KO/eta_0.dk" [];
  Dedukti.Check.ok ~regression:true ~filename:"tests/OK/nsteps3.dk" [];
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
  Test.run ()
