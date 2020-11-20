open Idllib.Syntax
open Source

(*
This module can translate Candid values (as parsed from the textual
representation) into Motoko values. This is a pragmatic translation which is
only used in the Candid test suite runner, to turn assertions about the
deserialized value into equalities between Motoko constants. It relies on the
fact that Candid and Motoko have very similar subtyping and overloading rules.

This is a best-effort thing; some Candid tests are not supported. The test
driver will downgrade them to a simple “does this deserialize” test.
*)

let parens s = "(" ^ s ^ ")"

let is_tuple_rec fs =
  let rec go next = function
    | [] -> true
    | f :: fs -> match (fst f.it).it with
      | Unnamed n when n = next -> go Lib.Uint32.(add n one) fs
      | Id n when n = next -> go Lib.Uint32.(add n one) fs
      | _ -> false
  in go Lib.Uint32.zero fs

(* Also compare with Mo_values.Show.show_val, which we cannot use here, because
   we don’t have the full type (although we could have that), and we have
   overloaded numeral here.
*)
let rec value v = match v.it with
  | BoolV true -> "true"
  | BoolV false -> "false"
  | NullV -> "null"
  | OptV v -> parens ("?" ^ value v)
  | VecV vs -> "[" ^ String.concat ", " (List.map value vs) ^ "]"
  | BlobV b -> "\"" ^ Mo_values.Value.Blob.escape b ^ "\""
    (* We are lazy and encode text values like blobs. Output not pretty, but works. *)
  | TextV s -> "\"" ^ Mo_values.Value.Blob.escape s ^ "\""
  | RecordV fs ->
    if is_tuple_rec fs
    then "(" ^ String.concat ", " (List.map (fun f -> value (snd f.it)) fs) ^ ")"
    else "{" ^ String.concat "; " (List.map rec_field fs) ^ "}"
  | VariantV f ->
    parens ("#" ^ Idl_to_mo.check_label (fst f.it) ^ parens (value (snd f.it)))
  | NumV n -> n
and
  rec_field f = Idl_to_mo.check_label (fst f.it) ^ " = " ^ value (snd f.it)

let args vs =
  parens (String.concat ", " (List.map value vs.it))
