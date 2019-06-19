module Flags :
sig
  val trace : bool ref
  val verbose : bool ref
  val print_depth : int ref
  val await_lowering : bool ref
  val async_lowering : bool ref
  val dump_parse : bool ref
  val dump_tc : bool ref
  val dump_lowering : bool ref
  val check_ir : bool ref
  val profile : bool ref
  val profile_verbose : bool ref
  val profile_file : string ref
  val profile_line_prefix : string ref
  val profile_field_names : string list ref
end

val check_files  : string list -> unit Diag.result
val check_string : string -> string -> unit Diag.result

val run_files           : string list -> unit option
val interpret_ir_files  : string list -> unit option
val run_files_and_stdin : string list -> unit option

type compile_mode = WasmMode | DfinityMode
type compile_result = Wasm_exts.CustomModule.extended_module Diag.result
val compile_string : compile_mode -> string -> string -> compile_result
val compile_files : compile_mode -> bool -> string list -> compile_result
