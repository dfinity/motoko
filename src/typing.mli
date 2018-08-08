open Type

(*
module Env : Map.S with type key = string

val lookup : 'a Env.t -> string -> 'a option
val union : 'a Env.t -> 'a Env.t -> 'a Env.t

*)
type val_env = (typ * mut) Env.t
type con_env = con Env.t
type kind_env = kind Con.Env.t

type context =
  {
    values : val_env;
    constructors : con_env;
    kinds : kind_env;
    labels : typ Env.t;
    returns : typ option;
    awaitable : bool
  }

val empty_context : context
val union_values : context -> val_env -> context
val union_constructors : context -> con_env -> context
val union_kinds : context -> kind_env -> context


exception KindError of Source.region * string
exception TypeError of Source.region * string

val check_prog : Syntax.prog -> val_env * con_env * kind_env
