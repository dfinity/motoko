open Mo_def
open Mo_types

open Type
open Scope

val initial_scope : scope
val infer_prog : scope -> Syntax.prog -> (typ * scope) Diag.result

val check_lib : scope -> Syntax.lib -> scope Diag.result
val check_actors : scope -> Syntax.prog list -> unit Diag.result
