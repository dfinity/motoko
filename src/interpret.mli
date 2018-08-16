module V = Value
module T = Type

exception Trap of Source.region * string

type val_env = V.rec_bind V.Env.t
type typ_env = T.con V.Env.t
type con_env = T.con_env
type lab_env = V.cont V.Env.t
type ret_env = V.cont option

type context =
  {
    vals : val_env;
    typs : typ_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool
  }

val interpret_prog : Syntax.prog -> (val_env -> V.value) -> unit

val get_last_region : unit -> Source.region
val get_last_context : unit -> context
