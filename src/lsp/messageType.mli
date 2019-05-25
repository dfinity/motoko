type t
  = Error
  | Warning
  | Info
  | Log
  | Unknown of int

val wrap : int -> t
val unwrap : t -> int
