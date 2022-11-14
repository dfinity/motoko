(** Describes a _stable_ way of referencing a definition. Stable in
   the sense that these references don't get invalidated by simple
   reformatting or small edits to the original source files like
   source ranges would, and also in a way that they have semantic
   meaning to a user. *)
type t =
  | XType of string
  | XValue of string
  | XNested of string * t
  | XClass of string * t
  | XFile of string * t option
  | XPackage of string * t option

val to_string : t -> string

(** Tries to _open_ an Xref so it can be nested further. Fails for
   Xref's that point at leaves like Types or Values *)
val extend : t -> (t -> t) option
