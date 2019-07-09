open Wasm_exts

open Printf

let name = "asc"
let version = "0.1"
let banner = "ActorScript " ^ version ^ " interpreter"
let usage = "Usage: " ^ name ^ " [option] [file ...]"


(* Argument handling *)

type mode = Default | Check | Compile | Run | Interact

let mode = ref Default
let args = ref []
let add_arg source = args := !args @ [source]

let set_mode m () =
  if !mode <> Default then begin
    eprintf "asc: multiple execution modes specified"; exit 1
  end;
  mode := m

let compile_mode = ref Pipeline.DfinityMode
let out_file = ref ""
let link = ref true
let interpret_ir = ref false
let gen_source_map = ref false

let argspec = Arg.align
[
  "-c", Arg.Unit (set_mode Compile), " compile programs to WebAssembly";
  "-r", Arg.Unit (set_mode Run), " interpret programs";
  "-i", Arg.Unit (set_mode Interact), " run interactive REPL (implies -r)";
  "--check", Arg.Unit (set_mode Check), " type-check only";
  "-v", Arg.Set Pipeline.Flags.verbose, " verbose output";
  "-p", Arg.Set_int Pipeline.Flags.print_depth, " set print depth";
  "-o", Arg.Set_string out_file, " output file";

  "--version",
    Arg.Unit (fun () -> printf "%s\n%!" banner; exit 0), " show version";
  "--map", Arg.Set gen_source_map, " output source map";

  "-t", Arg.Set Pipeline.Flags.trace, " activate tracing";
  "--package", Arg.Tuple [
                   Arg.Set_string Pipeline.Flags.package_name_temp ;
                   Arg.String begin fun package_url ->
                     (* push (package_name, package_url) onto the list. *)
                     Pipeline.Flags.package_urls := (
                       !Pipeline.Flags.package_name_temp,
                       package_url
                     ) :: ! Pipeline.Flags.package_urls
                     end
                 ], "<args> Specify a package-name-package-URL pair, separated by a space" ;
  "--profile", Arg.Set Pipeline.Flags.profile, " activate profiling counters in interpreters ";
  "--profile-file", Arg.Set_string Pipeline.Flags.profile_file, " set profiling output file ";
  "--profile-line-prefix", Arg.Set_string Pipeline.Flags.profile_line_prefix, " prefix each profile line with the given string ";
  "--profile-field",
  Arg.String (fun n -> Pipeline.Flags.(profile_field_names := n :: !profile_field_names)),
  " profile file includes the given field from the program result ";
  "-iR", Arg.Set interpret_ir, " interpret the lowered code";
  "-no-await", Arg.Clear Pipeline.Flags.await_lowering, " no await-lowering (with -iR)";
  "-no-async", Arg.Clear Pipeline.Flags.async_lowering, " no async-lowering (with -iR)";

  "-no-link", Arg.Clear link, " do not statically link-in runtime";
  "-no-dfinity-api",
    Arg.Unit (fun () -> compile_mode := Pipeline.WasmMode),
      " do not import the DFINITY system API";

  "-dp", Arg.Set Pipeline.Flags.dump_parse, " dump parse";
  "-dt", Arg.Set Pipeline.Flags.dump_tc, " dump type-checked AST";
  "-dl", Arg.Set Pipeline.Flags.dump_lowering, " dump intermediate representation ";
  "-no-check-ir", Arg.Clear Pipeline.Flags.check_ir, " do not check intermediate code";
]


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
  | Compile ->
    if !out_file = "" then begin
      match files with
      | [n] -> out_file := Filename.remove_extension (Filename.basename n) ^ ".wasm"
      | ns -> eprintf "asc: no output file specified"; exit 1
    end;
    let module_ = Diag.run Pipeline.(compile_files !compile_mode !link files) in
    let oc = open_out !out_file in
    let (source_map, wasm) = CustomModuleEncode.encode module_ in
    output_string oc wasm; close_out oc;

    if !gen_source_map then begin
      let source_map_file = !out_file ^ ".map" in
      let oc_ = open_out source_map_file in
      output_string oc_ source_map; close_out oc_
    end

(* Copy relevant flags into the profiler library's (global) settings.
   This indirection affords the profiler library an independence from the (hacky) Flags library.
   See also, this discussion:
   https://github.com/dfinity-lab/actorscript/pull/405#issuecomment-503326551
*)
let process_profiler_flags () =
  ProfilerFlags.profile             := !Pipeline.Flags.profile;
  ProfilerFlags.profile_verbose     := !Pipeline.Flags.profile_verbose;
  ProfilerFlags.profile_file        := !Pipeline.Flags.profile_file;
  ProfilerFlags.profile_line_prefix := !Pipeline.Flags.profile_line_prefix;
  ProfilerFlags.profile_field_names := !Pipeline.Flags.profile_field_names;
  ()

let () =
  (*
  Sys.catch_break true; - enable to get stacktrace on interrupt
  (useful for debugging infinite loops)
  *)
  Printexc.record_backtrace true;
  Arg.parse argspec add_arg usage;
  if !mode = Default then mode := (if !args = [] then Interact else Compile);
  process_profiler_flags () ;
  process_files !args
