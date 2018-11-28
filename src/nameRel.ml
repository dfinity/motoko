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

(* remove_range R S = { (x,y) | (x,y) ∈ R ∧ y ∉ S } *)
let remove_range : S.t -> t -> t = fun s ->
  M.map (fun s' -> S.diff s' s)

(* union R1 R2 = R1 ∪ R2 = { (x,y) | (x,y) ∈ R1 ∨ (x,y) ∈ R2 } *)
let union : t -> t -> t =
  M.union (fun _ s1 s2 -> Some (S.union s1 s2))

(* prod S1 S2 = S1 × S2 *)
let prod : S.t -> S.t -> t = fun s1 s2 ->
  S.fold (fun x rel -> M.add x s2 rel) s1 empty

(* comp R1 R2 = R1 ∘ R2 = { (x,z) | (x,y) ∈ R1 ∧ (y,z) ∈ R2 } *)
let comp : t -> t -> t = fun rel1 rel2 ->
  M.map (fun s -> S.fold (fun y -> S.union (lookup y rel2)) s S.empty) rel1
