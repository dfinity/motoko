(* This (or a type like this) could move into the IDL AST *)
type label = Nat of Lib.Uint32.t | Id of string
val escape : string -> string
val escape_num : Lib.Uint32.t -> string
val unescape : string -> label
val unescape_hash : string -> Lib.Uint32.t
