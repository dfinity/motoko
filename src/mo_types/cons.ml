(*
The kind field is a reference to break the recursion in open_binds,
and to allow the multiple passes in typing. These go through Type.set_kind
with additional safeguards.

Besides these two use-cases, the kind should not be mutated, and treated like
immutable data.

This module interface guarantees that constructors with the same stamp have the
same ref.
*)

type scope = string

type 'a con = {name : string; stamp : int * scope option; kind : 'a ref}
type 'a t = 'a con

module Stamps = Env.Make (struct
  type t = string * scope option

  let compare = Stdlib.compare
end)

type stamps = {stamps : int Stamps.t; scope : scope option}

let stamps : stamps ref = ref {stamps = Stamps.empty; scope = None}

let session ?scope f =
  let original = !stamps in
  stamps := {!stamps with scope};
  try let result = f () in
       stamps := original;
       result
  with e -> begin
     stamps := original;
     raise e
  end

let fresh_stamp name =
  let scope = !stamps.scope in
  let n = Lib.Option.get (Stamps.find_opt (name, scope) !stamps.stamps) 0 in
  stamps := {!stamps with stamps = Stamps.add (name, scope) (n + 1) !stamps.stamps};
  n, scope

let fresh name k = {name; stamp = fresh_stamp name; kind = ref k}
let clone c k = {c with stamp = fresh_stamp c.name; kind = ref k}

let kind c = !(c.kind)
let unsafe_set_kind c k = c.kind := k

let name c = c.name

let to_string show_stamps sep c =
  (* Filepaths may have non-parseable characters, so we convert them to hex. *)
  let escape_char c =
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> String.make 1 c
    | _ -> Printf.sprintf "%02X" (Char.code c)
  in
  let escape_filepath f =
    let n = String.length f in
    let buffer = Buffer.create (n * 2) in
    for i = 0 to n - 1 do
      Buffer.add_string buffer (escape_char f.[i])
    done;
    Buffer.contents buffer
  in
  if not show_stamps
  then c.name
  else
    Printf.sprintf
      "%s%s%i%s"
      c.name
      sep
      (fst c.stamp)
      (match snd c.stamp with
      | None -> ""
      | Some scope -> sep ^ escape_filepath scope)

let eq c1 c2 = c1.stamp = c2.stamp && c1.name = c2.name

let compare c1 c2 =
  match Int.compare (fst c1.stamp) (fst c2.stamp) with
  | 0 ->
    (match Option.compare String.compare (snd c1.stamp) (snd c2.stamp) with
    | 0 -> String.compare c1.name c2.name
    | ord -> ord)
  | ord -> ord
