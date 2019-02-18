type 'a con = {name : string; stamp : int; kind : 'a}
type 'a t = 'a con

module Stamps = Env.Make(String)

let stamps : int Stamps.t ref = ref Stamps.empty

let fresh name kind =
  let n =
    match Stamps.find_opt name !stamps with
    | Some n -> n
    | None -> 0
  in
  stamps := Stamps.add name (n + 1) !stamps;
  {name; stamp = n; kind}

let clone c k = {c with kind = k}

let kind c = c.kind
let name c = c.name

let to_string c =
  if c.stamp = 0 then c.name else Printf.sprintf "%s/%i" c.name c.stamp

let eq c1 c2 = (c1.name, c1.stamp) = (c2.name, c2.stamp)
let compare c1 c2 = compare (c1.name, c1.stamp) (c2.name, c2.stamp)
