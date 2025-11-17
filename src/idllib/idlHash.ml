(* IDL field hashes, called by unescape_hash *)
let idl_hash : string -> Lib.Uint32.t = fun s ->
  let open Lib.Uint32 in
  List.fold_left
    (fun s c -> add (mul s (of_int 223)) (of_int (Char.code c)))
    zero
    (Lib.String.explode s)
