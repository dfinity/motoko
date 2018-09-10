open Wasm.Values
open Wasm.Instance

exception Trap of Wasm.Source.region * string
exception Crash of Wasm.Source.region * string

val invoke : func_inst -> value list -> value list * int (* raises Trap *)
