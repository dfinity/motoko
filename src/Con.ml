type con = {name : string; stamp : int}
type t = con

let compare c1 c2 = compare c1.stamp c2.stamp

let stamp = ref 0

let fresh name =
  stamp := !stamp + 1;
  {name; stamp = !stamp}

let name c = c.name

let to_string c = Printf.sprintf "%s/%i" c.name c.stamp


module Env = Map.Make(struct type t = con let compare = compare end)
