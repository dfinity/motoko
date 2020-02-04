type parsed =
  | Package of (string * string)
  | Relative of string
  | Ic of string
  | IcAlias of string
  | Prim

val parse : string -> (parsed, string) result
val idl_basename_of_blob : string -> string
