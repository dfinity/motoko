type t
  = Error
  | Warning
  | Info
  | Log
  | Unknown

val wrap : int -> t
val unwrap : t -> int
