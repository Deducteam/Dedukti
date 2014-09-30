let sg = ref Signature.dummy

let init name = sg := Signature.make name
let get_name () = Signature.get_name !sg
let get_type l md id = Signature.get_type !sg l md id
let declare l id ty = Signature.declare !sg l id ty
let define l id te ty = Signature.define !sg l id te ty
let add_rules rs = Signature.add_rules !sg rs
let get_dtree l md id = Signature.get_dtree !sg l md id
let export () = Signature.export !sg
