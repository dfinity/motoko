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
  "-t", Arg.Set Flags.trace, " activate tracing";
  "-v", Arg.Set Flags.verbose, " verbose output";
  "-p", Arg.Set_int Flags.print_depth, " set print depth";
  "-a", Arg.Set Flags.await_lowering, " translate async/await (implies -r)";
  "-A", Arg.Clear Flags.async_lowering, " translate async<T> (implies -r)";
  "-dp", Arg.Set Flags.dump_parse, " dump parse";
  "-dl", Arg.Set Flags.dump_lowering, " dump lowering (requires -a)";
  "-o", Arg.Set_string out_file, " output file";
  "--version",
    Arg.Unit (fun () -> printf "%s\n" banner; exit 0), " show version";
  "--dfinity",
    Arg.Unit (fun () -> compile_mode := Pipeline.DfinityMode),
      " compile for dfinity";
  "--map", Arg.Set Flags.source_map, " output source map";
  "--disable-prelude", Arg.Clear Flags.prelude, " disable prelude";
]


(* Main *)

let exit_on_none = function
  | Some x -> x
  | None -> exit 1

let exit_on_failure = function
  | Ok x -> x
  | Error errs ->
    Pipeline.print_messages errs;
    exit 1

let process_files files : unit =
  match !mode with
  | Default ->
    assert false
  | Run ->
    ignore (exit_on_none Pipeline.(run_files initial_env files))
  | Interact ->
    printf "%s\n" banner;
    let env = exit_on_none Pipeline.(run_files initial_env files) in
    Pipeline.run_stdin env
  | Check ->
    let (_,_,_,msgs) = exit_on_failure Pipeline.(check_files initial_stat_env files) in
    Pipeline.print_messages msgs
  | Compile ->
    if !out_file = "" then begin
      match files with
      | [n] -> out_file := Filename.remove_extension (Filename.basename n) ^ ".wasm"
      | ns -> eprintf "asc: no output file specified"; exit 1
    end;
    let module_name = Filename.remove_extension (Filename.basename !out_file) in
    let module_ = exit_on_failure Pipeline.(compile_files !compile_mode files module_name) in
    let oc = open_out !out_file in
    let (source_map, wasm) = CustomModule.encode module_ in
    output_string oc wasm; close_out oc;

    if !Flags.source_map then begin
      let source_map_file = !out_file ^ ".map" in
      let oc_ = open_out source_map_file in
      output_string oc_ source_map; close_out oc_
    end

let () =
  Printexc.record_backtrace true;
  try
    Arg.parse argspec add_arg usage;
    if !mode = Default then mode := (if !args = [] then Interact else Compile);
    process_files !args
  with exn ->
    printf "%!";
    let at = Source.string_of_region (Interpret.get_last_region ()) in
    eprintf "%s: internal error, %s\n" at (Printexc.to_string exn);
    eprintf "\nLast environment:\n";
    Value.Env.iter (fun x d -> eprintf "%s = %s\n" x (Value.string_of_def d))
      Interpret.((get_last_env ()).vals);
    eprintf "\n";
    Printexc.print_backtrace stderr;
    eprintf "%!"
