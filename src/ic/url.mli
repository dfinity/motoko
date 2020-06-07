type parsed =
  | Package of (string * string)
  | Relative of string
  | Ic of string
  | IcAlias of string
  | Prim

val string_of_parsed : parsed -> string
val parse : string -> (parsed, string) result
val idl_basename_of_blob : string -> string
val encode_ic_url : string -> string
