type con = {name : string; stamp : int}
type t = con

module Stamps = Env.Make(String)

let stamps : int Stamps.t ref = ref Stamps.empty

let fresh name =
  let n =
    match Stamps.find_opt name !stamps with
    | Some n -> n
    | None -> 0
  in
  stamps := Stamps.add name (n + 1) !stamps;
  {name; stamp = n}

let name c = c.name

let to_string c =
  if c.stamp = 0 then c.name else Printf.sprintf "%s/%i" c.name c.stamp


module Env = Env.Make(struct type t = con let compare = compare end)
module Set = Set.Make(struct type t = con let compare = compare end)

let set_of_env m = Env.fold (fun v _ m -> Set.add v m) m Set.empty
