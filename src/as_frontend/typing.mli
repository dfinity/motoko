open As_types.Type
open As_types.Scope

val infer_prog : scope -> Syntax.prog -> (typ * scope) Diag.result
val check_library : scope -> Syntax.library -> scope Diag.result
