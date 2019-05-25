type t
  = Error
  | Warning
  | Information
  | Hint
  | Unknown of int

val wrap : int -> t
val unwrap : t -> int
