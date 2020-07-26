open Mo_types
open Mo_values
open Ir_def

module V = Value
module T = Type

type flags = {
  trace : bool;
  print_depth : int
}

exception Trap of Source.region * string

val interpret_prog : flags -> Ir.prog -> unit
