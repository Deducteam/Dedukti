let f_anon filename =
  let channel = open_in_bin filename in
    input_binary_int channel;
    let (md:Check.library_disk) = Marshal.from_channel channel in
      ()

let _ = Arg.parse [] f_anon ""
