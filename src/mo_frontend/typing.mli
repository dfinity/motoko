open Mo_def
open Mo_types

open Type
open Scope

val initial_scope : scope

val infer_prog : ?viper_mode:bool -> scope -> string option -> Async_cap.async_cap -> Syntax.prog -> (typ * scope) Diag.result

val check_lib : scope -> string option -> Syntax.lib -> scope Diag.result
val check_actors : ?viper_mode:bool -> ?check_actors:bool -> scope -> Syntax.prog list -> unit Diag.result
val check_stab_sig : scope -> Syntax.stab_sig -> (field list) Diag.result
