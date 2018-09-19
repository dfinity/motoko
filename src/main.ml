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

let argspec = Arg.align
[
  "-c", Arg.Unit (set_mode Compile), " compile programs to WebAssembly";
  "-r", Arg.Unit (set_mode Run), " interpret programs";
  "-i", Arg.Unit (set_mode Interact), " run interactive REPL (implies -r)";
  "--check", Arg.Unit (set_mode Check), " type-check only";
  "-t", Arg.Set Flags.trace, " activate tracing";
  "-v", Arg.Set Flags.verbose, " verbose output";
  "-p", Arg.Set_int Flags.print_depth, " set print depth";
  "--version",
    Arg.Unit (fun () -> printf "%s\n" banner; exit 0), " show version";
  "--dfinity", Arg.Set Flags.dfinity_mode, " compile for dfinity";
]


(* Main *)

let exit_on_failure = function
  | Some x -> x
  | None -> exit 1

let process_files ((senv, denv) as env) names : unit =
  match !mode with
  | Default ->
    assert false
  | Run ->
    ignore (exit_on_failure (Pipeline.run_files env names))
  | Interact ->
    printf "%s\n" banner;
    let env' = exit_on_failure (Pipeline.run_files env names) in
    Pipeline.run_stdin env'
  | Check ->
    ignore (exit_on_failure (Pipeline.check_files senv names));
  | Compile ->
    let module_ = exit_on_failure (Pipeline.compile_files senv names) in
    (* TBR: output to file *)
    Wasm.Print.module_ stdout 80 module_

let () =
  Printexc.record_backtrace true;
  try
    Arg.parse argspec add_arg usage;
    let env = Pipeline.init () in
    if !mode = Default then mode := (if !args = [] then Interact else Compile);
    process_files env !args
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
