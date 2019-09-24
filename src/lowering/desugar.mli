open As_def
open As_types
open Ir_def

module MakeDesugarer (Conf : sig val release : bool end) : sig
  val transform : Syntax.prog -> Ir.prog
  val transform_graph : Scope.lib_env -> Syntax.libraries -> Syntax.prog list -> Ir.prog
end
