type 'a t

module Unfold(M : Map.S) : sig
  type e = M.key
  val unfold : (e -> ('a * e list)) -> e -> 'a t
end

val canonicalize : 'a t -> 'a t

val fold : ('a -> 'b list -> 'b) -> (int -> 'b -> 'b) -> (int -> 'b) -> 'a t -> 'b
