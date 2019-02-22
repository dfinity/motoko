open Type

type val_env = typ Env.t
type typ_env = con Env.t
type con_env = ConSet.t

type scope =
  { val_env : val_env;
    typ_env : typ_env;
    con_env : con_env;
  }

val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

val check_prog : scope -> Syntax.prog -> scope Diag.result
val infer_prog : scope -> Syntax.prog -> (typ * scope) Diag.result
