open Types

(* *** Marshalisation *)

type dko = {
  version:string;
  dependencies:string list;
  table: rw_infos H.t;
}

let marshal lc file env deps =
  let obj = {
    version = Global.version ;
    dependencies = deps ;
    table = env ;
  } in
  try begin
    let out = open_out file in
      Marshal.to_channel out obj [Marshal.Closures] ;
      close_out out
  end with Sys_error msg ->
    Global.fail lc "Cannot export to '%s' (%s)." file msg

let unmarshal lc file =
  try begin
    let dko = open_in file in
    let obj:dko = Marshal.from_channel dko in
    let _ = close_in dko in
      if obj.version <> Global.version then
        Global.fail lc "Cannot load file '%s' (Bad version)." file
      else
        obj
  end with Sys_error msg ->
    Global.fail lc "Cannot load file '%s' (%s)." file msg
