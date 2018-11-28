open Type

type val_env = typ Env.t
type typ_env = con Env.t
type con_env = Type.con_env
type lab_env = typ Env.t
type ret_env = typ option

type scope = val_env * typ_env * con_env

type error = Error of Source.region * string
           | Warning of Source.region * string
type errors = error list

type env =
  { vals : val_env;
    typs : typ_env;
    cons : con_env;
    labs : lab_env;
    rets : ret_env;
    async : bool;
    pre : bool;
    errs : errors ref;
  }

val empty_env : env
val adjoin : env -> scope -> env
val adjoin_vals : env -> val_env -> env
val adjoin_typs : env -> typ_env -> con_env -> env

val check_prog : env -> Syntax.prog -> errors * scope option
val infer_prog : env -> Syntax.prog -> errors * (typ * scope) option
