(* Generative constructors *)

type t

val fresh : string -> t
val name : t -> string
val to_string : t -> string

module Env : Map.S with type key = t
