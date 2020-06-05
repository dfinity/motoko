open Mo_def
open Ir_def

type lib_or_class =
  | Lib of Syntax.lib
  | Compiled_class of (string (* file path *) * Mo_types.Type.typ * string (* module *))

val transform_graph :
  Syntax.prog (* prelude *) ->
  lib_or_class list (* libraries *) ->
  Syntax.prog list (* main program(s) *) ->
  Ir.prog
