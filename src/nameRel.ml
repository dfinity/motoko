(* A data structure for binary relations on names *)

module M = Env.Make(String)
module S = Set.Make(String)

type t = S.t M.t

let empty : t = M.empty

let cardinal : t -> int = fun m ->
  M.fold (fun _ s c -> S.cardinal s + c) m 0

let iter : (string -> string -> unit) -> t -> unit = fun f ->
  M.iter (fun x -> S.iter (f x))

let diag : string list -> t =
  List.fold_left (fun rel x -> M.add x (S.singleton x) rel) empty

let diag_set : S.t -> t = fun s ->
  S.fold (fun x rel -> M.add x (S.singleton x) rel) s empty

let cross : S.t -> S.t -> t = fun s1 s2 ->
  S.fold (fun x rel -> M.add x s2 rel) s1 empty

let lookup : string -> t -> S.t = fun k rel ->
  match M.find_opt k rel with
  | None -> S.empty
  | Some s -> s

(* dom R = { x | (x,y) ∈ R } *)
let dom : t -> S.t = fun rel ->
  M.fold (fun v s dom -> if S.is_empty s then dom else S.add v dom) rel S.empty

(* range R = { y | (x,y) ∈ R } *)
let range : t -> S.t = fun rel ->
  M.fold (fun _ s ran -> S.union s ran) rel S.empty

(* remove_range R S = { (x,y) | (x,y) ∈ R ∧ y ∉ S } *)
let remove_range : S.t -> t -> t = fun s ->
  M.map (fun s' -> S.diff s' s)

(* union R1 R2 = R1 ∪ R2 = { (x,y) | (x,y) ∈ R1 ∨ (x,y) ∈ R2 } *)
let union : t -> t -> t =
  M.union (fun _ s1 s2 -> Some (S.union s1 s2))

let unions : t list -> t =
  List.fold_left union empty

(* prod S1 S2 = S1 × S2 *)
let prod : S.t -> S.t -> t = fun s1 s2 ->
  S.fold (fun x rel -> M.add x s2 rel) s1 empty

(* Just a utility function *)
let set_bind f s =
  S.fold (fun y -> S.union (f y)) s S.empty

(* comp R1 R2 = R1 ∘ R2 = { (x,z) | (x,y) ∈ R1 ∧ (y,z) ∈ R2 } *)
let comp : t -> t -> t = fun rel1 rel2 ->
  M.map (set_bind (fun y -> lookup y rel2)) rel1

(* We can use a relation R to represent its transitive closure R⁺,
   and operations like dom, union, prod, comp work just fine.
   Only removing needs special care:*)

(* remove_range_trans S R = R' where remove_range S R⁺ = R'⁺ *)
let remove_range_trans : S.t -> t -> t = fun s rel ->
  M.map (set_bind (fun y -> if S.mem y s then S.diff (lookup y rel) s else S.singleton y)) rel

(* restricted_rtcl S R = { (x,z) | x ∈ S ∧ (x,z) ∈ R^* } *)
let restricted_rtcl : S.t -> t -> t = fun s rel ->
  let rec go prev =
    (* Add one iteration of R to the relation *)
    let next = union prev (comp prev rel) in
    (* Do we have a fixed point? *)
    if cardinal prev = cardinal next
    then prev
    else go next
  in go (diag_set s)


