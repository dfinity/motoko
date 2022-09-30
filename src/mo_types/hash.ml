(*
We hash field names using the following hash function,
which results in values [0..2^31-1]
*)

let hash : string -> int32 = fun s ->
  let open Int32 in
  let sum = ref zero in
  for i = 0 to String.length s - 1 do
    sum := add (mul (!sum) (of_int 223)) (of_int (Char.code (s.[i])))
  done;
  logand (shift_right_logical minus_one 1) (!sum)
