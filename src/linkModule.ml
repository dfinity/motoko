(*
The type for a dynamic library: A normal WebAssembly module
plus the dylink section.
*)

open Wasm.Ast
open Wasm.Decode

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


let decode name bs = raise (Code (Wasm.Source.no_region, ""))

(* The first argument specifies the global of the first module indicating the
start of free memory *)
let link heap_ptr m dm = m
