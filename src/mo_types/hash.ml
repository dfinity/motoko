(*
We hash field names using the following hash function,
which results in values [0..2^31-1]
*)

(* TODO: optimise *)
let hash : string -> int32 = fun s ->
  let open Int32 in
  logand (shift_right_logical minus_one 1) (
     List.fold_left
      (fun s c -> add (mul s (of_int 223)) (of_int (Char.code c)))
      zero
      (Lib.String.explode s)
  )

