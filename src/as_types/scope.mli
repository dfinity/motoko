open Type

type val_env = typ Env.t
type lib_env = typ Env.t
type typ_env = con Env.t
type con_env = ConSet.t

type obj_env = scope Env.t  (* internal object scopes *)

and scope =
  { val_env : val_env;
    lib_env : lib_env;
    typ_env : typ_env;
    con_env : con_env;
    obj_env : obj_env;
  }
and t = scope

val empty : scope
val adjoin : scope -> scope -> scope

val adjoin_val_env : scope -> val_env -> scope
val library : string -> typ -> scope
