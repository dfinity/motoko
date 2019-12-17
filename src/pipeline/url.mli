type parsed =
  | Package of (string * string)
  | Relative of string
  | Ic of string

val parse : string -> (parsed, string) result
val idl_basename_of_blob : string -> string
