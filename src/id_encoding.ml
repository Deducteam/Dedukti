let is_accepted c =
  let i = int_of_char c in
     (i >= 48 && i <= 57)
  || (i >= 65 && i <= 90)
  || (i >= 97 && i <= 122)


let transcode c =
  if c = '_' then "__" else
  Printf.sprintf "_%2x" (int_of_char c)

let encode_id s =
  let n = String.length s in
  let buf = Buffer.create n in
  for i = 0 to n - 1 do
    if is_accepted s.[i]
    then Buffer.add_char buf s.[i]
    else Buffer.add_string buf (transcode s.[i])
  done;
  Buffer.contents buf

