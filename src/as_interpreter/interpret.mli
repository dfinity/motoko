open As_def
open As_values
open As_types

module V = Value
module T = Type

type flags =
  { trace : bool;
    print_depth : int
  }

type scope =
  { val_env: V.def V.Env.t;
    lib_env: V.value V.Env.t;
  }

val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

exception Trap of Source.region * string

module type Conf = sig
  val release : unit -> bool
end

module MakeInterpreter (C : Conf) : sig
  val interpret_prog : flags -> scope -> Syntax.prog -> (V.value * scope) option
  val interpret_library : flags -> scope -> Syntax.library -> scope
end
