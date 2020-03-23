open Kernel
open Parsers
open Api

let  _ =
  let input = Parser.input_from_stdin (Basic.mk_mident "<top level>") in
  Format.printf "\tDedukti (%s)@.@." Version.version;
  Processor.handle_input input Processor.TopLevel;
