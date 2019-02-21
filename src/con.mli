(* Generative constructors *)

type 'a t

val fresh : string -> 'a -> 'a t
val kind : 'a t -> 'a
val name : 'a t -> string
val to_string : 'a t -> string
val eq : 'a t -> 'a t -> bool
val compare : 'a t -> 'a t -> int
val clone: 'a t -> 'a -> 'a t
