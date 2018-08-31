open Type

type val_env = typ Env.t
type typ_env = con Env.t
type con_env = Type.con_env
type lab_env = typ Env.t
type ret_env = typ option

type scope = val_env * typ_env * con_env

type context =
  { vals : val_env;
    typs : typ_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool;
    pre : bool;
  }

val empty_context : context
val adjoin : context -> scope -> context
val adjoin_vals : context -> val_env -> context
val adjoin_typs : context -> typ_env -> con_env -> context


exception Error of Source.region * string

val check_prog : context -> Syntax.prog -> scope (* raise Error *)
