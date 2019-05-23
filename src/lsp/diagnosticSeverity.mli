type t
  = Error
  | Warning
  | Information
  | Hint
  | Unknown

val wrap : int -> t
val unwrap : t -> int
