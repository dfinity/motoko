open Mo_def
open Mo_types

open Type
open Scope

val initial_scope : scope
val infer_prog : scope -> Async_cap.async_cap -> Syntax.prog -> (typ * scope) Diag.result

val check_lib : scope -> Syntax.lib -> scope Diag.result
val check_actors : scope -> Syntax.prog list -> unit Diag.result
val check_stab_sig : scope -> Syntax.stab_sig -> (field list) Diag.result

val heartbeat_type : typ
