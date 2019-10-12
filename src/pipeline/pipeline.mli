open As_def
open As_config
open As_types

type parse_fn = string -> (Syntax.prog * string) Diag.result
val parse_file: parse_fn
val parse_string: string -> parse_fn

val check_files  : string list -> unit Diag.result
val check_files' : parse_fn -> string list -> unit Diag.result
val check_string : string -> string -> unit Diag.result

val generate_idl : string list -> Idllib.Syntax.prog Diag.result

val initial_stat_env : Scope.scope
val chase_imports : parse_fn -> Scope.scope -> Resolve_import.S.t ->
  (Syntax.lib list * Scope.scope) Diag.result

val run_files           : string list -> unit option
val interpret_ir_files  : string list -> unit option
val run_files_and_stdin : string list -> unit option

type compile_result = Wasm_exts.CustomModule.extended_module Diag.result

val compile_string : Flags.compile_mode -> string -> string -> compile_result
val compile_files : Flags.compile_mode -> bool -> string list -> compile_result
