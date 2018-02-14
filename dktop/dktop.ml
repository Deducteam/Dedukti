let  _ =
  print_string "Welcome to Dedukti\n";
  let md = Basic.mk_mident "?top" in
    Env.init md;
    Scoping.name := md;
    Parser.handle_channel md Top.mk_entry stdin
