(* A data structure for binary relations on names *)

module M = Env.Make(String)
module S = Set.Make(String)

type t = S.t M.t

let empty : t = M.empty

let diag : string list -> t =
  List.fold_left (fun rel x -> M.add x (S.singleton x) rel) empty

let lookup : string -> t -> S.t = fun k rel ->
  match M.find_opt k rel with
  | None -> S.empty
  | Some s -> s

let remove_range : S.t -> t -> t = fun s ->
  M.map (fun s' -> S.diff s' s)
