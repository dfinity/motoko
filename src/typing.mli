open Type

type val_env = typ Env.t
type typ_env = con Env.t
type con_env = Type.con_env

(* TODO: Move to a module together with Severity *)
type message = Severity.t * Source.region * string
type messages = message list
type 'a messages_result = ('a * messages, messages) result

type scope =
  { val_env : val_env;
    typ_env : typ_env;
    con_env : con_env;
  }

val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

val check_prog : scope -> Syntax.prog -> scope messages_result
val infer_prog : scope -> Syntax.prog -> (typ * scope) messages_result
