open As_types

val check_pat : Syntax.pat -> Type.typ -> bool
val check_cases : Syntax.case list -> Type.typ -> bool
