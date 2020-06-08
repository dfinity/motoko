open Mo_def
open Ir_def

val transform_graph :
  Syntax.prog (* prelude *) ->
  Syntax.lib list (* libraries *) ->
  Syntax.prog list (* main program(s) *) ->
  Ir.prog
