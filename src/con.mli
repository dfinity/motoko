(* Generative constructors *)

type t

val fresh : string -> t
val name : t -> string
val to_string : t -> string

module Env : Env.S with type key = t
module Set : Set.S with type elt = t
