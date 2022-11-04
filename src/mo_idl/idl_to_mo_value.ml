open Idllib.Syntax
open Idllib.Exception
open Source
module T = Mo_types.Type

(*
This module can translate Candid values (as parsed from the textual
representation) into Motoko values. This is a pragmatic translation which is
only used in the Candid test suite runner, to turn assertions about the
deserialized value into equalities between Motoko constants. It relies on the
fact that Candid and Motoko have very similar subtyping and overloading rules.

Unfortunately, the overloading rules are different enough that we need
to consider the expected Motoko type `t` as well.
*)

let parens s = "(" ^ s ^ ")"
let parens_comma ss = parens (String.concat ", " ss)
let brackets_comma ss = "[" ^ String.concat ", " ss ^ "]"

let is_tuple_rec fs =
  if fs = [] then false else
  let rec go next = function
    | [] -> true
    | f :: fs -> match (fst f.it).it with
      | Unnamed n when n = next -> go Lib.Uint32.(add n one) fs
      | Id n when n = next -> go Lib.Uint32.(add n one) fs
      | _ -> false
  in go Lib.Uint32.zero fs

(* We are lazy and encode text values like blobs. Output not pretty, but works. *)
let text_lit s = "\"" ^ Mo_values.Value.Blob.escape s ^ "\""

let find_typ tfs f =
  try T.lookup_val_field (Idl_to_mo.check_label (fst (f.it))) tfs
  with Invalid_argument _ ->
    raise (UnsupportedCandidFeature
      (Diag.error_message f.at "M0164" "import" "unknown record or variant label in textual representation"))

(* Also compare with Mo_values.Show.show_val, which we cannot use here, because
   we don’t have the full type (although we could have that), and we have
   overloaded numerals here.
*)
let rec value v t =
  match v.it, T.normalize t with
  | BoolV true, _ -> "true"
  | BoolV false, _ -> "false"
  | NullV, _ -> "null"
  | OptV v, T.Opt t1 -> parens ("?" ^ value v t1)
  | VecV vs, T.Array t1 -> brackets_comma (List.map (fun v -> value v t ) vs)
  | BlobV b, T.Prim T.Blob -> text_lit b
  | BlobV b, T.Array _ ->
    brackets_comma (List.of_seq (Seq.map (fun c -> Printf.sprintf "%d" (Char.code c)) (String.to_seq b)))
  | TextV s, _ -> text_lit s
  | RecordV fs, T.Obj (T.Object, tfs) ->
    "{" ^ String.concat "; " (List.map (fun f ->
      Idl_to_mo.check_label (fst f.it) ^ " = " ^ value (snd f.it) (find_typ tfs f)
    ) fs) ^ "}"
  | RecordV fs, T.Tup ts ->
    (* this will only line up if the record is written in tuple short hand order *)
    parens_comma (List.map2 (fun f t -> value (snd f.it) t) fs ts)
  | VariantV f, T.Variant tfs ->
    let t1 = find_typ tfs f in
    if T.normalize t1 = T.unit
    then parens ("#" ^ Idl_to_mo.check_label (fst f.it))
    else parens ("#" ^ Idl_to_mo.check_label (fst f.it) ^ parens (value (snd f.it) t1))
  | NumV n, _ -> n
  | ServiceV s, _ ->
    parens ("actor " ^ text_lit s)
  | FuncV (s, m), _ ->
    Printf.sprintf "(actor %s : actor { %s : %s }).%s"
      (text_lit s)
      (Idllib.Escape.escape_method Source.no_region m)
      (T.string_of_typ t)
      (Idllib.Escape.escape_method Source.no_region m)
  | PrincipalV s, _ ->
    "Prim.principalOfActor" ^ parens ("actor " ^ text_lit s ^ " : actor {}")
  | _ -> raise (UnsupportedCandidFeature
    (Diag.error_message v.at "M0165" "import" "odd expected type"))

let args vs ts = parens_comma (List.map2 value vs.it ts)
