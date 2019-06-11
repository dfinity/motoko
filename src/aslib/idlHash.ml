(* IDL field hashes *)
let idl_hash : string -> int32 = fun s ->
  let open Int32 in
  List.fold_left
    (fun s c -> add (mul s 223l) (of_int (Char.code c)))
    0l
    (* TODO: also unescape the string, once #465 is approved *)
    (Lib.String.explode s)
