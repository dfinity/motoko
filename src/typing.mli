open Type

type val_env = typ Env.t
type typ_env = con Env.t
type con_env = Type.con_env

type scope =
  { val_env : val_env;
    typ_env : typ_env;
    con_env : con_env;
  }

val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

type error = Source.region * string
exception Error of error list

val check_prog : scope -> Syntax.prog -> scope (* raise Error *)
val infer_prog : scope -> Syntax.prog -> typ * scope (* raise Error *)
