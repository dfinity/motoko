open Wasm_exts
open Mo_config

open Printf

let name = "moc"
let banner = "Motoko compiler (revision " ^ Source_id.id ^ ")"
let usage = "Usage: " ^ name ^ " [option] [file ...]"


(* Argument handling *)

type mode = Default | Check | Compile | Run | Interact | Idl | PrintDeps

let mode = ref Default
let args = ref []
let add_arg source = args := !args @ [source]

let set_mode m () =
  if !mode <> Default then begin
    eprintf "moc: multiple execution modes specified"; exit 1
  end;
  mode := m

let out_file = ref ""
let link = ref true
let interpret_ir = ref false
let gen_source_map = ref false

let argspec = Arg.align [
  "-c", Arg.Unit (set_mode Compile), " compile programs to WebAssembly";
  "-g", Arg.Set Flags.debug_info, " generate source-level debug information";
  "-r", Arg.Unit (set_mode Run), " interpret programs";
  "-i", Arg.Unit (set_mode Interact), " run interactive REPL (implies -r)";
  "--check", Arg.Unit (set_mode Check), " type-check only";
  "--idl", Arg.Unit (set_mode Idl), " generate IDL spec";
  "--print-deps", Arg.Unit (set_mode PrintDeps), " prints the dependencies for a given source file";
  "-o", Arg.Set_string out_file, " output file";

  "-v", Arg.Set Flags.verbose, " verbose output";
  "-p", Arg.Set_int Flags.print_depth, " set print depth";
  "--hide-warnings", Arg.Clear Flags.print_warnings, " hide warnings"; ]

  @ Args.error_args

  @ [

  "--version",
    Arg.Unit (fun () -> printf "%s\n%!" banner; exit 0), " show version";
  "--map", Arg.Set gen_source_map, " output source map";

  "-t", Arg.Set Flags.trace, " activate tracing in interpreters"]

  @ Args.package_args

  @ [
  "--profile", Arg.Set Flags.profile, " activate profiling counters in interpreters ";
  "--profile-file", Arg.Set_string Flags.profile_file, " set profiling output file ";
  "--profile-line-prefix", Arg.Set_string Flags.profile_line_prefix, " prefix each profile line with the given string ";
  "--profile-field",
  Arg.String (fun n -> Flags.(profile_field_names := n :: !profile_field_names)),
  " profile file includes the given field from the program result ";
  "-iR", Arg.Set interpret_ir, " interpret the lowered code";
  "-no-await", Arg.Clear Flags.await_lowering, " no await-lowering (with -iR)";
  "-no-async", Arg.Clear Flags.async_lowering, " no async-lowering (with -iR)";

  "-no-link", Arg.Clear link, " do not statically link-in runtime";
  "-no-system-api",
    Arg.Unit (fun () -> Flags.(compile_mode := WasmMode)),
      " do not import any system API";
  "-wasi-system-api",
    Arg.Unit (fun () -> Flags.(compile_mode := WASIMode)),
      " use the WASI system API (wasmtime)";
  "-ref-system-api",
    Arg.Unit (fun () -> Flags.(compile_mode := RefMode)),
      " use the future DFINITY system API (ic-ref-run)";
  (* TODO: bring this back (possibly with flipped default)
           as soon as the multi-value `wasm` library is out.
  "-multi-value", Arg.Set Flags.multi_value, " use multi-value extension";
  "-no-multi-value", Arg.Clear Flags.multi_value, " avoid multi-value extension";
   *)

  "-dp", Arg.Set Flags.dump_parse, " dump parse";
  "-dt", Arg.Set Flags.dump_tc, " dump type-checked AST";
  "-dl", Arg.Set Flags.dump_lowering, " dump intermediate representation ";
  "-no-check-ir", Arg.Clear Flags.check_ir, " do not check intermediate code";
  "--release",
  Arg.Unit
    (fun () -> Flags.release_mode := true),
      " ignore debug expressions in source";
  "--debug",
  Arg.Unit
    (fun () -> Flags.release_mode := false),
      " respect debug expressions in source (the default)";
]


let set_out_file files ext =
  if !out_file = "" then begin
    match files with
    | [n] -> out_file := Filename.remove_extension (Filename.basename n) ^ ext
    | _ -> eprintf "moc: no output file specified"; exit 1
  end

let set_compilation_unit = function
  | [n] ->
    if Compile = !mode
    then Flags.(compilation_unit := n; compilation_dir := ".")
  | _ -> assert false

(* Main *)

let exit_on_none = function
  | None -> exit 1
  | Some x -> x

let process_files files : unit =
  match !mode with
  | Default ->
    assert false
  | Run ->
    if !interpret_ir
    then exit_on_none (Pipeline.interpret_ir_files files)
    else exit_on_none (Pipeline.run_files files)
  | Interact ->
    printf "%s\n%!" banner;
    exit_on_none (Pipeline.run_files_and_stdin files)
  | Check ->
    Diag.run (Pipeline.check_files files)
  | Idl ->
    set_out_file files ".did";
    let prog = Diag.run (Pipeline.generate_idl files) in
    let oc = open_out !out_file in
    let idl_code = Idllib.Arrange_idl.string_of_prog prog in
    output_string oc idl_code; close_out oc
  | Compile ->
    set_out_file files ".wasm";
    set_compilation_unit files;
    let module_ = Diag.run Pipeline.(compile_files !Flags.compile_mode !link files) in
    let oc = open_out !out_file in
    let (source_map, wasm) = CustomModuleEncode.encode module_ in
    output_string oc wasm; close_out oc;

    if !gen_source_map then begin
      let source_map_file = !out_file ^ ".map" in
      let oc_ = open_out source_map_file in
      output_string oc_ source_map; close_out oc_
    end
  | PrintDeps ->
     match files with
     | [file] -> Pipeline.print_deps file
     | _ ->
        (eprintf "--print-deps expects exactly one source file as an argument";
         exit 1)

(* Copy relevant flags into the profiler library's (global) settings.
   This indirection affords the profiler library an independence from the (hacky) Flags library.
   See also, this discussion:
   https://github.com/dfinity-lab/motoko/pull/405#issuecomment-503326551
*)
let process_profiler_flags () =
  ProfilerFlags.profile             := !Flags.profile;
  ProfilerFlags.profile_verbose     := !Flags.profile_verbose;
  ProfilerFlags.profile_file        := !Flags.profile_file;
  ProfilerFlags.profile_line_prefix := !Flags.profile_line_prefix;
  ProfilerFlags.profile_field_names := !Flags.profile_field_names;
  ()

let () =
  (*
  Sys.catch_break true; - enable to get stacktrace on interrupt
  (useful for debugging infinite loops)
  *)
  Printexc.record_backtrace true;
  Arg.parse argspec add_arg usage;
  if !mode = Default then mode := (if !args = [] then Interact else Compile);
  Flags.compiled := (!mode = Compile);
  process_profiler_flags () ;
  process_files !args
