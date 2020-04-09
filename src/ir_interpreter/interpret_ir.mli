open Mo_types
open Mo_values
open Ir_def

module V = Value
module T = Type

type flags = {
  trace : bool;
  print_depth : int
}

type state
val initial_state : unit -> state

type scope
val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

exception Trap of Source.region * string

val interpret_prog : flags -> state -> scope -> Ir.prog -> scope
