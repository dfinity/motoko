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
let expect_fail_pats = ref []

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
  "--expect-fail", Arg.String (fun s -> expect_fail_pats := s :: !expect_fail_pats), " tests expected to fail";
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
let mo_of_test tenv test : (string * (* should_not_trap *) bool) option =
  let deser t x =
    "(prim \"deserialize\" : Blob -> " ^ Type.string_of_typ (Type.seq t) ^ ") " ^
    "\"" ^ Value.Blob.escape x ^ "\"" in
  let equal e1 e2     = "assert (" ^ e1 ^ " == " ^ e2 ^ ")\n" in
  let not_equal e1 e2 = "assert (" ^ e1 ^ " != " ^ e2 ^ ")\n" in
  let ignore e = "ignore (" ^ e ^ ")\n" in

  let defs =
    String.concat "" (List.map (fun (n,candid_typ) ->
      let mo_typ = Idl_to_mo.check_typ tenv candid_typ in
      "type " ^ n ^ " = " ^ Type.string_of_typ mo_typ ^ ";\n"
    ) (Typing.Env.bindings tenv)) ^ "\n" in

  let typ = Idl_to_mo.check_typs tenv (test.it.ttyp) in
  match test.it.assertion with
  | ParsesAs (true, BinaryInput i)
  | ParsesEqual (_, BinaryInput i, TextualInput _)
  | ParsesEqual (_, TextualInput _, BinaryInput  i)
  -> Some (defs ^ ignore (deser typ i), true)
  | ParsesAs (false, BinaryInput i)
  -> Some (defs ^ ignore (deser typ i), false)
  | ParsesEqual (true, BinaryInput i1, BinaryInput i2)
  -> Some (defs ^ equal (deser typ i1) (deser typ i2), true)
  | ParsesEqual (false, BinaryInput i1, BinaryInput i2)
  -> Some (defs ^ not_equal (deser typ i1) (deser typ i2), true)
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

  let expected_fail =
    let rs = List.map Str.regexp !expect_fail_pats in
    fun name ->
      List.exists (fun r ->
        try ignore(Str.search_forward r name 0); true
        with Not_found -> false
      ) rs
  in

  let count = ref 0 in
  let count_ok = ref 0 in
  let count_fail = ref 0 in
  let count_unexpected_ok = ref 0 in
  let count_expected_fail = ref 0 in
  let count_skip = ref 0 in

  let files = Sys.readdir !test_dir in
  Array.sort compare files;
  Array.iter (fun base ->
    match Lib.String.chop_suffix ".test.did" base with
    | Some name ->
      Printf.printf "Parsing %s ...\n%!" base;
      let tests = Diag.run (Pipeline.parse_test_file (Filename.concat !test_dir base)) in
      let tenv = Diag.run (Typing.check_tdecs Typing.empty_scope tests.it.tdecs) in

      List.iter (fun test ->
        let testname =
          match test.it.desc with
          | None -> Printf.sprintf "%s:%d" name test.at.left.line
          | Some n -> Printf.sprintf "%s:%d %s" name test.at.left.line n in

        count := !count + 1;
        if filter testname then begin
          Printf.printf "%s ...%!" testname;
          (* generate test program *)
          match mo_of_test tenv test with
          | None -> Printf.printf " ignored.\n"
          | Some (src, must_not_trap) ->
            (* Printf.printf "\n%s" src *)
            Unix.putenv "MOC_UNLOCK_PRIM" "yesplease";
            write_file "tmp.mo" src;
            match run_cmd "moc -wasi-system-api tmp.mo -o tmp.wasm" with
            | (false, stdout, stderr) ->
              count_fail := !count_fail + 1;
              Printf.printf " compilation failed:\n%s%s\n" stdout stderr
            | (true, _, _) ->
              if expected_fail testname
              then
              begin
                Printf.printf " should not be ok:";
                match must_not_trap, run_cmd "timeout 10s wasmtime --disable-cache --cranelift tmp.wasm" with
                | true, (true, _, _)
                | false, (false, _, _) ->
                  count_unexpected_ok := !count_unexpected_ok + 1;
                  Printf.printf " ok!\n";
                | true, (false, stdout, stderr) ->
                  count_expected_fail := !count_expected_fail + 1;
                  Printf.printf " fail (unexpected trap)!\n%s%s\n" stdout stderr;
                | false, (true, _, _) ->
                  count_expected_fail := !count_expected_fail + 1;
                  Printf.printf " fail (unexpected pass)!\n";
              end else
                match must_not_trap, run_cmd "wasmtime --disable-cache --cranelift tmp.wasm" with
                | true, (true, _, _)
                | false, (false, _, _) ->
                  count_ok := !count_ok + 1;
                  Printf.printf " ok!\n";
                | true, (false, stdout, stderr) ->
                  count_fail := !count_fail + 1;
                  Printf.printf " fail (unexpected trap)!\n%s%s\n" stdout stderr;
                | false, (true, _, _) ->
                  count_fail := !count_fail + 1;
                  Printf.printf " fail (unexpected pass)!\n";
        end else count_skip := !count_skip + 1;
      ) tests.it.tests;
    | None ->
      match Lib.String.chop_suffix ".did" base with
      | Some _ -> Printf.printf "Ignoring file %s ...\n" base;
      | None -> ()
  ) files;

  Printf.printf "%d tests: %d skipped, %d ok, %d failed, %d unexpected ok, %d expected fail\n"
    !count !count_skip !count_ok !count_fail !count_unexpected_ok !count_expected_fail;
  if !count_fail + !count_unexpected_ok > 0
  then exit 1
  else exit 0

