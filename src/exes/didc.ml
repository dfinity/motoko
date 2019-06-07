open Idllib
open Printf

let name = "idlc"
let version = "0.1"
let banner = "Interface Description Language (IDL) " ^ version ^ " interpreter"
let usage = "Usage: " ^ name ^ " [option] [file ...]"


(* Argument handling *)

type mode = Default | Check | Js

let mode = ref Default
let args = ref []
let add_arg source = args := !args @ [source]

let set_mode m () =
  if !mode <> Default then begin
    eprintf "asc: multiple execution modes specified"; exit 1
  end;
  mode := m

let out_file = ref ""

let argspec = Arg.align
[
  "--js", Arg.Unit (set_mode Js), " output Javascript binding";
  "--check", Arg.Unit (set_mode Check), " type-check only";
  "-v", Arg.Set Flags.verbose, " verbose output";
  "-dp", Arg.Set Flags.dump_parse, " dump parse";
  "-o", Arg.Set_string out_file, " output file";
  "--version",
    Arg.Unit (fun () -> printf "%s\n" banner; exit 0), " show version";
]


(* Main *)

let exit_on_failure = function
  | Ok x -> x
  | Error errs ->
    Diag.print_messages errs;
    exit 1

let process_files files : unit =
  match !mode with
  | Default ->
     assert false
  | Check ->
     let (_, msgs) = exit_on_failure (Pipeline.(check_file (List.hd files))) in
     Diag.print_messages msgs
  | Js ->
     let out = exit_on_failure (Pipeline.(compile_js_file (List.hd files))) in
     Buffer.contents out |> print_endline
     
let print_exn exn =
  Printf.printf "%!";
  Printf.eprintf "Internal error, %s\n" (Printexc.to_string exn);
  Printexc.print_backtrace stderr;
  Printf.eprintf "%!"
       
let () =
  (* 
  Sys.catch_break true; - enable to get stacktrace on interrupt
  (usefull for debugging infinite loops)
  *)
  Printexc.record_backtrace true;
  try
    Arg.parse argspec add_arg usage;
    if !mode = Default then mode := Check;
    process_files !args
  with exn ->
    print_exn exn
