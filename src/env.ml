module type S =
sig
  include Map.S

  exception Clash of key

  val from_list : (key * 'a) list -> 'a t
  val from_list2 : key list -> 'a list -> 'a t
  val adjoin : 'a t -> 'a t -> 'a t
  val disjoint_union : 'a t -> 'a t -> 'a t (* raises Clash *)
  val disjoint_add : key -> 'a -> 'a t -> 'a t (* raises Clash *)
end

module Make(X : Map.OrderedType) : S with type key = X.t =
struct
  include Map.Make(X)

  exception Clash of key

  let from_list kxs = List.fold_left (fun env (k, x) -> add k x env) empty kxs
  let from_list2 ks xs = List.fold_left2 (fun env k x -> add k x env) empty ks xs
  let adjoin env1 env2 = union (fun _ x1 x2 -> Some x2) env1 env2
  let disjoint_union env1 env2 = union (fun k _ _ -> raise (Clash k)) env1 env2
  let disjoint_add k x env = disjoint_union env (singleton k x)
                           
end
