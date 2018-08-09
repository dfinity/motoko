module type S =
sig
  include Map.S

  exception Overlap of key

  val adjoin : 'a t -> 'a t -> 'a t
  val disjoint_union : 'a t -> 'a t -> 'a t (* raises Overlap *)
  val disjoint_add : key -> 'a -> 'a t -> 'a t (* raises Overlap *)
end

module Make(X : Map.OrderedType) : S with type key = X.t =
struct
  include Map.Make(X)

  exception Overlap of key

  let adjoin env1 env2 = union (fun _ x1 x2 -> Some x2) env1 env2
  let disjoint_union env1 env2 = union (fun k _ _ -> raise (Overlap k)) env1 env2
  let disjoint_add k x env = disjoint_union env (singleton k x)
end
