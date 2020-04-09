module type S =
sig
  include Set.S

  exception Clash of elt

  val disjoint_add : elt -> t -> t (* raises Clash *)
  val disjoint_union : t -> t -> t (* raises Clash *)
end

module Make(X : Set.OrderedType) : S with type elt = X.t =
struct
  include Set.Make(X)

  exception Clash of elt

  let disjoint_add e set = if mem e set then raise (Clash e) else add e set
  let disjoint_union set1 set2 = fold (fun e s -> disjoint_add e s) set2 set1
end
