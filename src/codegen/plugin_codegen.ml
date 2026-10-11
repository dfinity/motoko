(* Versioned, native compiler extension interface. Generators run at compile
   time; their output is linked into the canister as ordinary Wasm code.
   The heap operations below share the compiler's forwarding and allocation
   barriers. A generated operation must not introduce a GC safepoint. *)
open Mo_types
open Wasm_exts.Types
module G = InstrList

let abi_version = 1

exception Unsupported of string

module type Backend = sig
  type env
  val compile_unboxed_const : int64 -> G.t
  val compile_const_32 : int32 -> G.t
  val compile_unboxed_zero : G.t
  val compile_unboxed_one : G.t
  val compile_add_const : int64 -> G.t
  val compile_mul_const : int64 -> G.t
  val compile_eq_const : int64 -> G.t
  val compile_comparison : Wasm_exts.Ast.I64Op.relop -> G.t
  val compile_comparison_f32 : Wasm_exts.Ast.F32Op.relop -> G.t
  val compile_comparison_f64 : Wasm_exts.Ast.F64Op.relop -> G.t
  val new_local : env -> string -> G.t * G.t
  val new_local_ : env -> value_type -> string -> G.t * G.t * int32
  val from_0_to_n : env -> (G.t -> G.t) -> G.t
  val call : env -> string -> string -> G.t
  val alloc_blob : env -> G.t -> G.t
  val unbox_float32 : env -> G.t
  val box_float32 : env -> G.t
  val buffer_size : env -> Type.typ -> G.t
  val candid_primitive : Type.typ -> int32 option
  val sort_by_hash : Type.field list -> (Lib.Uint32.t * Type.field) list
  module E : sig
    val if0 : G.t -> G.t -> G.t
    val if1 : value_type -> G.t -> G.t -> G.t
    val trap_with : env -> string -> G.t
    val then_trap_with : env -> string -> G.t
    val else_trap_with : env -> string -> G.t
  end
  module Func : sig
    type sharing = Always | Never
    val share_code : sharing -> env -> string ->
      (string * value_type) list -> value_type list ->
      (env -> G.t list -> G.t) -> G.t
    val share_code1 : sharing -> env -> string ->
      string * value_type -> value_type list -> (env -> G.t -> G.t) -> G.t
    val share_code3 : sharing -> env -> string ->
      (string * value_type) * (string * value_type) * (string * value_type) ->
      value_type list -> (env -> G.t -> G.t -> G.t -> G.t) -> G.t
  end
  module Blob : sig
    val len : env -> G.t
    val payload_ptr_unskewed : env -> G.t
    val of_ptr_size : env -> G.t
  end
  module Arr : sig
    val len : env -> G.t
    val unsafe_idx : env -> G.t
    val for_each : env -> G.t -> G.t -> (G.t -> G.t) -> G.t
    (* Length and index are unboxed i64. Initialize slots before calling the
       element generator, so an early error return cannot leave invalid heap
       fields. No element generator may introduce a GC safepoint. *)
    val tabulate : env -> G.t -> (G.t -> G.t) -> G.t
  end
  module Tuple : sig
    val load_n : env -> int64 -> G.t
    val from_stack : env -> int -> G.t
    val compile_unit : env -> G.t
  end
  module Object : sig
    val load_idx_raw : env -> string -> G.t
    val lit_raw : env -> (string * (unit -> G.t)) list -> G.t
  end
  module Variant : sig
    val inject : env -> string -> G.t -> G.t
    val project : env -> G.t
    val test_is : env -> string -> G.t
    val get_variant_tag : env -> G.t
    val hash_variant_label : env -> string -> int64
  end
  module Opt : sig
    val null_lit : env -> G.t
    val inject : env -> Type.typ -> G.t -> G.t
    val is_some : env -> G.t
    val project : env -> Type.typ -> G.t
  end
  module Text : sig
    val size : env -> G.t
    val lit : env -> string -> G.t
    val of_ptr_size : env -> G.t
    val to_blob : env -> G.t
  end
  module Principal : sig
    (* Consume raw pointer and byte count. The caller validates <= 29 bytes. *)
    val of_ptr_size : env -> G.t
  end
  module Float : sig
    val unbox : env -> G.t
    val box : env -> G.t
  end
  module BoxedWord64 : sig
    val unbox : env -> Type.prim -> G.t
    val box : env -> Type.prim -> G.t
  end
  module TaggedSmallWord : sig
    val lsb_adjust : Type.prim -> G.t
    val msb_adjust : Type.prim -> G.t
    val tag : env -> Type.prim -> G.t
    val lsb_adjust_codepoint : env -> G.t
  end
  module BitTagged : sig
    val if_tagged_scalar : env -> value_type list -> G.t -> G.t -> G.t
  end
  module BigNum : sig
    val from_word64 : env -> G.t
    val from_signed_word64 : env -> G.t
    val compile_add : env -> G.t
    val compile_mul : env -> G.t
    val compile_neg : env -> G.t
    (* A scalar bridge for arbitrary precision numbers; [true] means signed.
       Encoding consumes a vanilla integer and returns a Blob. Decoding
       consumes raw pointer and size of validated (S)LEB bytes. *)
    val to_leb128 : env -> bool -> G.t
    val of_leb128 : env -> bool -> G.t
  end
  module BigNumLibtommath : sig
    val fits_unsigned_bits : env -> int -> G.t
  end
  module Bool : sig
    val lit : bool -> G.t
    val from_rts_int32 : G.t
    val to_rts_int32 : G.t
  end
  module I32Leb : sig val compile_leb128_size : G.t -> G.t end
  module Strm : sig
    val name_for : string -> Type.typ list -> string
    val reserve : env -> G.t -> int64 -> G.t
    val write_byte : env -> G.t -> G.t -> G.t
    val write_word_32 : env -> G.t -> G.t -> G.t
    val write_word_leb : env -> G.t -> G.t -> G.t
    val write_bignum_leb : env -> G.t -> G.t -> G.t
    val write_bignum_sleb : env -> G.t -> G.t -> G.t
    val write_blob : env -> G.t -> G.t -> G.t
    val write_text : env -> G.t -> G.t -> G.t
  end
end

module type Generator = sig
  module Make (B : Backend) : sig
    val compile : B.env -> Mo_values.Compute_plugin.specialization -> G.t
  end
end

type runtime_function = string * value_type list * value_type list
type plugin = {
  abi : int;
  name : string;
  runtime : string;
  imports : runtime_function list;
  bindings : (Mo_values.Compute_plugin.binding * (module Generator)) list;
}

type installed = { plugin : plugin; runtime_path : string }
let directory = ref None
let plugins : installed list ref = ref []

let register plugin =
  if plugin.abi <> abi_version then
    raise (Unsupported (Printf.sprintf
      "plugin %S requires compute plugin ABI %d, \
      but this compiler supports ABI %d"
      plugin.name plugin.abi abi_version));
  if List.exists (fun p -> p.plugin.name = plugin.name) !plugins then
    invalid_arg ("duplicate compute plugin: " ^ plugin.name);
  let dir = match !directory with
    | Some dir -> dir
    | None -> invalid_arg "compute plugins must be loaded with --plugin"
  in
  List.iter (fun (binding, _) -> Mo_values.Compute_plugin.register binding)
    plugin.bindings;
  plugins := !plugins @ [{plugin;
    runtime_path = Filename.concat dir plugin.runtime}]

let load loader path =
  let file = if Sys.is_directory path then
      Filename.concat path "plugin.cmxs" else path in
  directory := Some (Filename.dirname file);
  Fun.protect ~finally:(fun () -> directory := None) (fun () -> loader file)

let installed () = !plugins
let module_name plugin = "compute." ^ plugin.name

let generator id =
  List.find_map (fun p -> List.find_map (fun (binding, generator) ->
    if binding.Mo_values.Compute_plugin.id = id then Some generator else None)
    p.plugin.bindings) !plugins
