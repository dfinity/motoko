module V = Value
module T = Type

type val_env = V.def V.Env.t
type lab_env = V.value V.cont V.Env.t
type ret_env = V.value V.cont option

type scope = val_env

type env =
  { vals : val_env;
    labs : lab_env;
    rets : ret_env;
    async : bool
  }

val empty_env : env
val adjoin : env -> scope -> env


exception Trap of Source.region * string

val interpret_prog : env -> Syntax.prog -> V.value option * val_env

val get_last_region : unit -> Source.region
val get_last_env : unit -> env
