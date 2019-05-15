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

let compile_mode = ref Pipeline.WasmMode
let out_file = ref ""

let argspec = Arg.align
[
  "-c", Arg.Unit (set_mode Compile), " compile programs to WebAssembly";
  "-r", Arg.Unit (set_mode Run), " interpret programs";
  "-i", Arg.Unit (set_mode Interact), " run interactive REPL (implies -r)";
  "--check", Arg.Unit (set_mode Check), " type-check only";
  "-v", Arg.Set Flags.verbose, " verbose output";
  "-p", Arg.Set_int Flags.print_depth, " set print depth";
  "-o", Arg.Set_string out_file, " output file";

  "--version",
    Arg.Unit (fun () -> printf "%s\n" banner; exit 0), " show version";
  "--dfinity",
    Arg.Unit (fun () -> compile_mode := Pipeline.DfinityMode),
      " compile for dfinity";
  "--map", Arg.Set Flags.source_map, " output source map";

  "-t", Arg.Set Flags.trace, " activate tracing";
  "--profile", Arg.Set Flags.profile, " activate profiling counters in interpreters ";
  "--profile-file", Arg.Set_string Flags.profile_file, " set profiling output file ";
  "--profile-line-prefix", Arg.Set_string Flags.profile_line_prefix, " prefix each profile line with the given string ";
  "--profile-field",
  Arg.String (fun n -> Flags.profile_field_names := n :: !Flags.profile_field_names),
  " suffix each profile line with the given string ";
  "-iR", Arg.Set Flags.interpret_ir, " interpret the lowered code";
  "-no-await", Arg.Clear Flags.await_lowering, " no await-lowering (with -iR)";
  "-no-async", Arg.Clear Flags.async_lowering, " no async-lowering (with -iR)";

  "-no-link", Arg.Clear Flags.link, " do not statically link-in runtime";

  "-dp", Arg.Set Flags.dump_parse, " dump parse";
  "-dt", Arg.Set Flags.dump_tc, " dump type-checked AST";
  "-dl", Arg.Set Flags.dump_lowering, " dump intermediate representation ";
  "--disable-prelude", Arg.Clear Flags.prelude, " disable prelude";
]


(* Main *)

let exit_on_failure = function
  | Ok x -> x
  | Error errs ->
    Diag.print_messages errs;
    exit 1

let run_diag = function
  | Ok ((), warns) ->
    Diag.print_messages warns;
    exit 0
  | Error errs ->
    Diag.print_messages errs;
    exit 1

let process_files files : unit =
  match !mode with
  | Default ->
    assert false
  | Run ->
    if !Flags.interpret_ir
    then run_diag (Pipeline.interpret_ir_files files)
    else run_diag (Pipeline.run_files files)
  | Interact ->
    printf "%s\n" banner;
    run_diag (Pipeline.run_files_and_stdin files)
  | Check ->
    let ((), msgs) = exit_on_failure (Pipeline.check_files files) in
    Diag.print_messages msgs
  | Compile ->
    if !out_file = "" then begin
      match files with
      | [n] -> out_file := Filename.remove_extension (Filename.basename n) ^ ".wasm"
      | ns -> eprintf "asc: no output file specified"; exit 1
    end;
    let module_ = exit_on_failure
      Pipeline.(compile_files !compile_mode !(Flags.link) files) in
    let oc = open_out !out_file in
    let (source_map, wasm) = CustomModuleEncode.encode module_ in
    output_string oc wasm; close_out oc;

    if !Flags.source_map then begin
      let source_map_file = !out_file ^ ".map" in
      let oc_ = open_out source_map_file in
      output_string oc_ source_map; close_out oc_
    end

let () =
  (*
  Sys.catch_break true; - enable to get stacktrace on interrupt
  (useful for debugging infinite loops)
  *)
  Printexc.record_backtrace true;
  Arg.parse argspec add_arg usage;
  if !mode = Default then mode := (if !args = [] then Interact else Compile);
  process_files !args
