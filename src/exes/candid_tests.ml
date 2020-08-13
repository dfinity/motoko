(* This programs run the candid test suite at
   https://github.com/dfinity/candid/tree/master/test

*)
open Idllib
open Source
open Idllib.Syntax
open Mo_idl
open Printf
open Mo_types
open Mo_values

let name = "candid-tests"
let version = "0.1"
let banner = "Candid test suite runner " ^ version ^ ""
let usage = "Usage: " ^ name ^ " -i path/to/candid/test"

(* Argument handling *)

let test_dir = ref ""
let pattern = ref ""

let print_banner () =
  printf "%s\n" banner;
  exit 0

let usage_err s =
  eprintf "%s: %s\n" name s;
  eprintf "%s\n" usage;
  exit 1

let argspec = Arg.align
[
  "-i", Arg.Set_string test_dir, " candid test directory";
  "-p", Arg.Set_string pattern, " test selector (substring/regex)";
  "--version", Arg.Unit print_banner, " show version";
]

(* IO *)

let load_file f =
  let ic = open_in_bin f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let write_file f s =
  let oc_ = open_out f in
  output_string oc_ s;
  close_out oc_

(* Turning a test case into a motoko program *)
let mo_of_test test : (string * (* should_not_trap *) bool) option =
  let deser t x =
    "(prim \"deserialize\" : Blob -> " ^ Type.string_of_typ (Type.seq t) ^ ") " ^
    "\"" ^ Value.Blob.escape x ^ "\"" in
  let equal e1 e2     = "assert (" ^ e1 ^ " == " ^ e2 ^ ")\n" in
  let not_equal e1 e2 = "assert (" ^ e1 ^ " != " ^ e2 ^ ")\n" in
  let ignore e = "ignore (" ^ e ^ ")\n" in

  let typ = Idl_to_mo.check_typs Typing.empty_scope (test.it.ttyp) in
  match test.it.assertion with
  | ParsesAs (true, BinaryInput i)
  | ParsesEqual (_, BinaryInput i, TextualInput _)
  | ParsesEqual (_, TextualInput _, BinaryInput  i)
  -> Some (ignore (deser typ i), true)
  | ParsesAs (false, BinaryInput i)
  -> Some (ignore (deser typ i), false)
  | ParsesEqual (true, BinaryInput i1, BinaryInput i2)
  -> Some (equal (deser typ i1) (deser typ i2), true)
  | ParsesEqual (false, BinaryInput i1, BinaryInput i2)
  -> Some (not_equal (deser typ i1) (deser typ i2), true)
  | ParsesAs (_, TextualInput _)
  | ParsesEqual (_, TextualInput _, TextualInput _)
  -> None

let run_cmd cmd : (bool * string * string) =
  let  (stdout_c, stdin_c, stderr_c) =
    Unix.open_process_full cmd (Unix.environment ()) in
  (* shoddy reading of all pipes *)
  let s = Bytes.create 10000 in
  let n = input stdout_c s 0 10000 in
  let stdout = Bytes.sub_string s 0 n in
  let n = input stderr_c s 0 10000 in
  let stderr = Bytes.sub_string s 0 n in
  match Unix.close_process_full (stdout_c, stdin_c, stderr_c) with
  | Unix.WEXITED 0 -> (true, stdout, stderr)
  | _ -> (false, stdout, stderr)


(* Main *)
let () =
  Arg.parse argspec (fun _ -> usage_err "no arguments expected") usage;
  if !test_dir = "" then usage_err "no candid test directory specified";


  let filter =
    if !pattern = ""
    then fun name -> true
    else
      let r = Str.regexp !pattern in
      fun name ->
        try ignore(Str.search_forward r name 0); true
        with Not_found -> false
  in

  let files = Sys.readdir !test_dir in
  Array.sort compare files;
  Array.iter (fun base ->
    match Lib.String.chop_suffix ".test.did" base with
    | Some name ->
      Printf.printf "Parsing %s...\n%!" base;
      let tests = Diag.run (Pipeline.parse_test_file (Filename.concat !test_dir base)) in
      if (tests.it.tdecs <> [])
      then (Printf.eprintf "Definitions not yet supported\n"; exit 1);

      List.iter (fun test ->
        let testname =
          match test.it.desc with
          | None -> Printf.sprintf "%s:%d" name test.at.left.line
          | Some n -> Printf.sprintf "%s:%d %s" name test.at.left.line n in

        if filter testname then begin
          Printf.printf "%s...%!" testname;
          (* generate test program *)
          match mo_of_test test with
          | None -> Printf.printf " ignored.\n"
          | Some (src, must_not_trap) ->
            (* Printf.printf "\n%s" src *)
            Unix.putenv "MOC_UNLOCK_PRIM" "yesplease";
            write_file "tmp.mo" src;
            match run_cmd "moc -wasi-system-api tmp.mo -o tmp.wasm" with
            | (false, stdout, stderr) ->
              Printf.printf " compilation failed:\n%s%s\n" stdout stderr
            | (true, _, _) ->
              match must_not_trap, run_cmd "wasmtime tmp.wasm" with
              | true, (true, _, _)
              | false, (false, _, _) ->
                Printf.printf " ok!\n";
              | true, (false, stdout, stderr) ->
                Printf.printf " fail (unexpected trap)!\n%s%s\n" stdout stderr;
              | false, (true, _, _) ->
                Printf.printf " fail (unexpected pass)!\n";
        end
      ) tests.it.tests;
    | None ->
      match Lib.String.chop_suffix ".did" base with
      | Some _ -> Printf.printf "Ignoring file %s...\n" base;
      | None -> ()
  ) files
