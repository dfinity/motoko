(*module Flags :
sig)
  val trace : bool ref
  val verbose : bool ref
  val print_depth : int ref
  val release_mode : bool ref
  val await_lowering : bool ref
  val async_lowering : bool ref
  val dump_parse : bool ref
  val dump_tc : bool ref
  val dump_lowering : bool ref
  val check_ir : bool ref
  val package_urls : (string * string) list ref
  val profile : bool ref
  val profile_verbose : bool ref
  val profile_file : string ref
  val profile_line_prefix : string ref
  val profile_field_names : string list ref
(end*)

(*module Flags = struct*)
  let trace = ref false
  let verbose = ref false
  let print_depth = ref 2
  let release_mode = ref false
  let await_lowering = ref true
  let async_lowering = ref true
  let dump_parse = ref false
  let dump_tc = ref false
  let dump_lowering = ref false
  let check_ir = ref true
  let package_urls : (string * string) list ref = ref []
  let profile = ref false
  let profile_verbose = ref false
  let profile_file = ref "profiling-counters.csv"
  let profile_line_prefix = ref ""
  let profile_field_names : string list ref = ref []
(*end *)(* Flags *)
