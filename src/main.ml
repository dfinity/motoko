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
  "--map", Arg.Set Flags.source_map, " output source map";

  "-t", Arg.Set Flags.trace, " activate tracing";
  "-iR", Arg.Set Flags.interpret_ir, " interpret the lowered code";
  "-no-await", Arg.Clear Flags.await_lowering, " no await-lowering (with -iR)";
  "-no-async", Arg.Clear Flags.async_lowering, " no async-lowering (with -iR)";

  "-no-link", Arg.Clear Flags.link, " do not statically link-in runtime";
  "-no-dfinity-api",
    Arg.Unit (fun () -> compile_mode := Pipeline.WasmMode),
      " do not import the DFINITY system API";

  "-dp", Arg.Set Flags.dump_parse, " dump parse";
  "-dt", Arg.Set Flags.dump_tc, " dump type-checked AST";
  "-dl", Arg.Set Flags.dump_lowering, " dump intermediate representation ";
  "--disable-prelude", Arg.Clear Flags.prelude, " disable prelude";
]


(* Main *)

let process_files files : unit =
  match !mode with
  | Default ->
    assert false
  | Run ->
    if !Flags.interpret_ir
    then Diag.run (Pipeline.interpret_ir_files files)
    else Diag.run (Pipeline.run_files files)
  | Interact ->
    printf "%s\n" banner;
    Diag.run (Pipeline.run_files_and_stdin files)
  | Check ->
    Diag.run (Pipeline.check_files files)
  | Compile ->
    if !out_file = "" then begin
      match files with
      | [n] -> out_file := Filename.remove_extension (Filename.basename n) ^ ".wasm"
      | ns -> eprintf "asc: no output file specified"; exit 1
    end;
    let module_ = Diag.run Pipeline.(compile_files !compile_mode !(Flags.link) files) in
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
