type 'a t

val unfold : ('e -> ('a * 'e list)) -> 'e -> 'a t

val canonicalize : 'a t -> 'a t

val fold : ('a -> 'b list -> 'b) -> (int -> 'b -> 'b) -> (int -> 'b) -> 'a t -> 'b
