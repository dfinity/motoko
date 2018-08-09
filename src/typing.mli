open Type

type val_env = (typ * mut) Env.t
type typ_env = con Env.t
type con_env = Type.con_env

type context =
  {
    vals : val_env;
    typs : typ_env;
    cons : con_env;
    labels : typ Env.t;
    returns : typ option;
    awaitable : bool
  }

val empty_context : context
val adjoin_vals : context -> val_env -> context
val adjoin_typs : context -> typ_env -> context
val adjoin_cons : context -> con_env -> context


exception KindError of Source.region * string
exception TypeError of Source.region * string

val check_prog : Syntax.prog -> val_env * typ_env * con_env
