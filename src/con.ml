(*
The kind field is a reference to break the recursion in open_binds,
and to allow the multiple passes in typing. These go through Type.set_kind
with additional safeguards.

Besides these two use-cases, the kind should not be mutated, and treated like
immutable data.

This module interface guarantees that constructors with the same stamp have the
same ref.
*)

type 'a con = {name : string; stamp : int; kind : 'a ref}
type 'a t = 'a con

module Stamps = Env.Make(String)

let stamps : int Stamps.t ref = ref Stamps.empty

let fresh_stamp name =
  let n =
    match Stamps.find_opt name !stamps with
    | Some n -> n
    | None -> 0
  in
  stamps := Stamps.add name (n + 1) !stamps;
  n

let fresh name k =
  {name; stamp = fresh_stamp name; kind = ref k}
let clone c k =
  { c with stamp = fresh_stamp c.name; kind = ref k } (* Does not change the stamp! *)

let name c = c.name

let kind c = !(c.kind)
let unsafe_set_kind c k = c.kind := k

let to_string c =
  if c.stamp = 0 then c.name else Printf.sprintf "%s/%i" c.name c.stamp

let compare c1 c2 = compare (c1.name, c1.stamp) (c2.name, c2.stamp)
let eq c1 c2 = (c1.name, c1.stamp) = (c2.name, c2.stamp)

