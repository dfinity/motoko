(*
The type for a dynamic library: A normal WebAssembly module
plus the dylink section.
*)

open Wasm.Ast

type dylink = {
  memorysize : int32;
  memoryalignment : int32;
  tablesize : int32;
  tablealignment : int32;
  needed_dynlibs : string list;
}

type dylink_module = {
  module_ : module_;
  dylink :  dylink
}

val decode : string -> string -> dylink_module (* raises Code *)

(* The first argument specifies the global of the first module indicating the
start of free memory *)
val link : int32 -> module_ -> dylink_module -> module_
