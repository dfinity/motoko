module type S =
sig
  include Map.S

  module Dom : Dom.S with type elt = key

  exception Clash of key

  val dom : 'a t -> Dom.t
  val keys : 'a t -> key list
  val from_list : (key * 'a) list -> 'a t
  val from_list2 : key list -> 'a list -> 'a t
  val adjoin : 'a t -> 'a t -> 'a t
  val disjoint_add : key -> 'a -> 'a t -> 'a t (* raises Clash *)
  val disjoint_union : 'a t -> 'a t -> 'a t (* raises Clash *)
  val disjoint_unions : 'a t list -> 'a t (* raises Clash *)
end

module Make(X : Map.OrderedType) : S with type key = X.t =
struct
  include Map.Make(X)

  module Dom = Dom.Make(X)

  exception Clash of key

  let dom env = List.fold_left (fun s (x, _) -> Dom.add x s) Dom.empty (bindings env)
  let keys env = List.map fst (bindings env)
  let from_list kxs = List.fold_left (fun env (k, x) -> add k x env) empty kxs
  let from_list2 ks xs = List.fold_left2 (fun env k x -> add k x env) empty ks xs
  let adjoin env1 env2 = union (fun _ x1 x2 -> Some x2) env1 env2
  let disjoint_union env1 env2 = union (fun k _ _ -> raise (Clash k)) env1 env2
  let disjoint_unions envs = List.fold_left disjoint_union empty envs
  let disjoint_add k x env = disjoint_union env (singleton k x)

end
