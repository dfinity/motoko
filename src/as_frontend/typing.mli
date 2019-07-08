open As_def
open As_types

open Type
open Scope

val infer_prog : scope -> Syntax.prog -> (typ * scope) Diag.result
val check_library : scope -> Syntax.library -> scope Diag.result
