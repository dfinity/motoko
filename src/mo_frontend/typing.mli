open Mo_def
open Mo_types

open Type
open Scope

val initial_scope : scope

val infer_prog
  :  ?enable_type_recovery:bool
  -> scope
  -> string option
  -> Async_cap.async_cap
  -> Syntax.prog
  -> (typ * scope) Diag.result

val check_lib : scope -> string option -> Syntax.lib -> scope Diag.result
val check_actors : ?check_actors:bool -> scope -> Syntax.prog list -> unit Diag.result

val check_stab_sig : scope -> Syntax.stab_sig -> Type.stab_sig Diag.result

type ctx_dot_candidate =
  { module_name : lab option;
    path : Syntax.exp;
    arg_ty : typ;
    func_ty : typ;
    inst : typ list;
  }

val resolve_dot_candidates : lib_env -> val_env -> typ -> (string * ctx_dot_candidate) list
