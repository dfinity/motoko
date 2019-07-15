open As_def
open As_types

val check_pat : Syntax.pat -> Type.typ -> string list
val check_cases : Syntax.case list -> Type.typ -> string list
