let drun_config_path = "../drun.json5"

let read_file file_path =
  let ch = open_in file_path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

module StringSet = Set.Make (String)

(** Collect all .mo file names in a .drun file *)
let collect_drun_mo_files (drun_file_path : string) : StringSet.t =
  let mo_file_regex = Re.compile (Re.Perl.re "[^ ]*mo") in
  let file_contents = read_file drun_file_path in
  StringSet.of_list (Re.matches mo_file_regex file_contents)

(** Run a drun test specified as a .mo file *)
let drun_mo_test (mo_file_path : string) : unit Alcotest.test_case =
  let open Alcotest in
  test_case (Printf.sprintf "drun: %s\n" mo_file_path) `Quick (fun () -> ())

let rec print_list (strs : string list) =
  match strs with
  | [] -> ()
  | e :: l ->
      Printf.printf "%s\n" e;
      print_list l

(** - Replace file paths like `$test_name/x.mo` in a file with
    `$out/x.$runner.wasm`

      (`test_name`, `out`, and `runner` are arguments to this function)

    - Insert "create" at the beginning of the script

    - Replace "$ID"s in the drun script with the canister id that drun gets
      when installing the canister. This is currently hard-coded in this
      function and needs to be in sync with drun and/or replica.
    *)
let prepare_drun_script (file_path : string) (out_dir : string)
    (test_name : string) (runner : string) : string =
  (* Step 1: replace .mo file paths with .wasm file paths *)
  let re_string = Printf.sprintf "%s\\/([^\\s]+)\\.mo" test_name in
  let re = Re.compile (Re.Perl.re re_string) in
  let file_contents = read_file file_path in

  let replace group =
    let match_ = Re.Group.get group 1 in
    Printf.sprintf "%s/%s.%s.wasm" out_dir match_ runner
  in

  let script = Re.replace re ~f:replace file_contents in

  (* Step 2: Insert "create" *)
  let script = "create\n" ^ script in

  (* Step 3: replace "$ID" with the actual canister id *)
  (* This needs to be in sync with drun and/or replica *)
  let canister_id = "rwlgt-iiaaa-aaaaa-aaaaa-cai" in
  let re = Re.Str.regexp_string "$ID" in

  Re.Str.global_replace re canister_id script

(** Run a drun test specified as a .drun file *)
let drun_drun_test (drun_file_path : string) : unit Alcotest.test_case =
  Alcotest.test_case (Printf.sprintf "drun: %s" drun_file_path) `Quick
    (fun () ->
      let mo_files = collect_drun_mo_files drun_file_path in
      Printf.printf "Drun .mo files: ";
      print_list (List.of_seq (StringSet.to_seq mo_files));

      let test_name =
        Filename.remove_extension (Filename.basename drun_file_path)
      in

      StringSet.iter
        (fun mo_file ->
          (* TODO: ../run-drun part should be gone *)
          let mo_file_path = Printf.sprintf "../run-drun/%s" mo_file in
          Printf.printf "Compiling mo file: %s\n" mo_file_path;

          let mo_base = Filename.remove_extension (Filename.basename mo_file) in
          let out_dir = Printf.sprintf "_out/%s" test_name in
          let moc_cmd =
            Printf.sprintf "moc --hide-warnings -c %s -o %s/%s.drun.wasm\n"
              mo_file_path out_dir mo_base
          in

          Printf.printf "Compiling %s: `%s`" mo_file moc_cmd;

          let mkdir_exit = Sys.command (Printf.sprintf "mkdir -p %s" out_dir) in
          Alcotest.(check int) "mkdir exit code" 0 mkdir_exit;

          let moc_exit = Sys.command moc_cmd in
          Alcotest.(check int) "moc exit code" 0 moc_exit;

          let drun_script =
            prepare_drun_script drun_file_path out_dir test_name "drun"
          in

          Printf.printf "Drun script: %s\n" drun_script;

          let drun_path = Printf.sprintf "%s/%s.drun.drun" out_dir test_name in
          let drun_out = open_out drun_path in
          Printf.fprintf drun_out "%s\n" drun_script;
          close_out drun_out;

          let drun_cmd =
            Printf.sprintf "drun -c %s --extra-batches 1 %s" drun_config_path
              drun_path
          in

          Printf.printf "Running %s\n" drun_cmd;

          let drun_exit = Sys.command drun_cmd in
          Alcotest.(check int) "drun exit code" 0 drun_exit;

          (* TODO: Compare output files *)
          ())
        mo_files)

(** Scan directory drun/ for tests. Only the top-level files are tests. .drun
    files have .mo files in subdirectories. *)
let collect_drun_tests () : unit Alcotest.test_case list =
  let dir_name = "../run-drun" in

  let file_filter file_name =
    let extension = Filename.extension file_name in
    if extension = ".mo" then
      Some (drun_mo_test (Filename.concat dir_name file_name))
    else if extension = ".drun" then
      Some (drun_drun_test (Filename.concat dir_name file_name))
    else None
  in

  List.of_seq (Seq.filter_map file_filter (Array.to_seq (Sys.readdir dir_name)))

let collect_tests () : (string * unit Alcotest.test_case list) list =
  [ ("drun", collect_drun_tests ()) ]

let () =
  let open Alcotest in
  run "Motoko compiler tests" (collect_tests ())
