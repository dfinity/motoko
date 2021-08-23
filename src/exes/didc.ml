open Idllib
open Printf

let name = "didc"
let banner = "Candid compiler " ^ Source_id.banner
let usage = "Usage: " ^ name ^ " [option] [file ...]"


(* Argument handling *)

type mode = Default | Check | Js | PrettyPrint

let mode = ref Default
let args = ref []
let add_arg source = args := !args @ [source]

let set_mode m () =
  if !mode <> Default then begin
    eprintf "didc: multiple execution modes specified"; exit 1
  end;
  mode := m

let out_file = ref ""

let argspec =
[
  "--js", Arg.Unit (set_mode Js), " output JavaScript binding";
  "--check", Arg.Unit (set_mode Check), " type-check only";
  "--pp", Arg.Unit (set_mode PrettyPrint), " Pretty print did file";
  "-v", Arg.Set Flags.verbose, " verbose output";
  "-dp", Arg.Set Flags.dump_parse, " dump parse";
  "-o", Arg.Set_string out_file, "<file>  output file";
  "--version",
    Arg.Unit (fun () -> printf "%s\n" banner; exit 0), " show version";
]


(* Main *)

let process_file file : unit =
  match !mode with
  | Default ->
     assert false
  | Check ->
     ignore (Diag.run (Pipeline.check_file file))
  | PrettyPrint ->
     let (ast, _, _) = Diag.run (Pipeline.check_file file) in
     printf "%s" (Idllib.Arrange_idl.string_of_prog ast);
  | Js ->
     if !out_file = "" then
         out_file := Filename.remove_extension (Filename.basename file) ^ ".js";
     let buf = Diag.run Pipeline.(compile_js_file file) in
     if Buffer.length buf = 0 then begin
         eprintf "No actor found, cannot generate JS bindings"; exit 1
     end;
     let oc = open_out !out_file in
     Buffer.add_string buf "\n";
     Buffer.output_buffer oc buf;
     close_out oc

let print_exn exn =
  Printf.printf "%!";
  Printf.eprintf "Internal error, %s\n" (Printexc.to_string exn);
  Printexc.print_backtrace stderr;
  Printf.eprintf "%!"

let () =
  (*
  Sys.catch_break true; - enable to get stacktrace on interrupt
  (useful for debugging infinite loops)
  *)
  Printexc.record_backtrace true;
  try
    Arg.parse argspec add_arg usage;
    if !mode = Default then mode := Js;
    match !args with
    | [file] -> process_file file
    | _ -> eprintf "didc can only take one .did file\n"; exit 1
  with exn ->
    print_exn exn
