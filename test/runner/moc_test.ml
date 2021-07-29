open Utils

let drun_config_path = "../drun.json5"

(** Collect all .mo file names in a .drun file *)
let collect_drun_mo_files (drun_file_path : string) : StringSet.t =
  let mo_file_regex = Re.compile (Re.Perl.re "[^ ]*mo") in
  let file_contents = read_file drun_file_path in
  StringSet.of_list (Re.matches mo_file_regex file_contents)

(** Typecheck a mo file, assert that the output is as expected. This function
    is used for tests that expect to not type check. *)
let typecheck_mo_file (mo_file_path : string) : unit =
  let mo_file_contents = read_file mo_file_path in

  let file_name = Filename.remove_extension (Filename.basename mo_file_path) in

  let moc_extra_args =
    String.concat " "
      (collect_lines_starting_with mo_file_contents "//MOC-FLAG")
  in

  let moc_extra_envs =
    String.concat " " (collect_lines_starting_with mo_file_contents "//MOC-ENV")
  in

  Printf.printf "moc_extra_args for typechecking: %s\n" moc_extra_args;
  Printf.printf "moc_extra_envs for typechecking: %s\n" moc_extra_envs;

  let output_file_path = Printf.sprintf "../run-drun/_out/%s.tc" file_name in
  let moc_cmd =
    Printf.sprintf "env %s moc %s --check %s > %s" moc_extra_envs moc_extra_args
      mo_file_path output_file_path
  in
  Printf.printf "Type checking: %s\n" moc_cmd;

  let moc_exit = Sys.command moc_cmd in

  (* If there's an expected output file compare it with the actual output *)
  let expected_output_path =
    Printf.sprintf "../run-drun/ok/%s/%s.tc.ok" file_name file_name
  in

  let expected_output =
    try Some (read_file expected_output_path) with Sys_error _ -> None
  in

  (match expected_output with
  | None -> ()
  | Some expected_output ->
      let actual_output = read_file output_file_path in

      if expected_output <> actual_output then
        diff ~expected_output_path ~actual_output_path:output_file_path);

  (* If exit code is not 0, compare exit code with the value in "...tc.ret.ok"
     file. This part is a bit hacky, ported from the old shell script without
     changes. *)
  if moc_exit <> 0 then (
    (* There should be a ".tc.ret.ok" file *)
    let ret_file_path =
      Printf.sprintf "../run-drun/ok/%s.tc.ret.ok" file_name
    in

    let ret_file_contents = read_file ret_file_path in

    (* File contents will be like "Return code ...", drop the prefix *)
    let prefix = "Return code " in
    let prefix_len = String.length prefix in
    let expected_ret_str =
      String.trim
        (String.sub ret_file_contents prefix_len
           (String.length ret_file_contents - prefix_len))
    in
    Printf.printf "expected_ret_str: [%s]\n" expected_ret_str;
    let expected_ret = int_of_string expected_ret_str in

    Alcotest.(check int) "moc --check exit code" expected_ret moc_exit)

(** Run a drun test specified as a .mo file *)
let drun_mo_test (mo_file_path : string) : string * unit Alcotest.test_case list
    =
  let test_name = Filename.remove_extension (Filename.basename mo_file_path) in

  let mo_file_contents = read_file mo_file_path in

  let tests =
    (* If the test has a ".tc.ok" file then we only typecheck it as it's expect
       to not type check *)
    let typecheck_file_path =
      Printf.sprintf "../run-drun/ok/%s.tc.ok" test_name
    in
    if Sys.file_exists typecheck_file_path then
      [
        Alcotest.test_case "tc" `Quick (fun () ->
            typecheck_mo_file mo_file_path);
      ]
    else
      let _moc_extra_args =
        String.concat " "
          (collect_lines_starting_with mo_file_contents "//MOC-FLAG")
      in

      let _moc_extra_envs =
        String.concat " "
          (collect_lines_starting_with mo_file_contents "//MOC-ENV")
      in

      (* Otherwise run with the interpreters and drun *)
      []
    (* TODO *)
  in

  (test_name, tests)

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

(** Create a drun script for a mo file *)
let prepare_drun_script_for_mo (_file_path : string) = ()

(** Transform drun output to make it easier to check for correctness. So far we
    only remove canister ids in debug prints and replace them with the old
    "debug.print:" prefix. *)
let normalize_drun_output (output : string) : string =
  let canister_id_re = Re.compile (Re.Perl.re "\\[Canister [0-9a-z\\-]+\\]") in
  Re.replace_string canister_id_re ~by:"debug.print:" output

(** Run a drun test specified as a .drun file *)
let drun_drun_test (drun_file_path : string) :
    string * unit Alcotest.test_case list =
  let test_name =
    Filename.remove_extension (Filename.basename drun_file_path)
  in

  let out_dir = Printf.sprintf "_out/%s" test_name in

  ( test_name,
    [
      Alcotest.test_case "" `Quick (fun () ->
          let mo_files = collect_drun_mo_files drun_file_path in
          Printf.printf "Drun .mo files: ";
          print_list (List.of_seq (StringSet.to_seq mo_files));

          (* Compile .mo files *)
          StringSet.iter
            (fun mo_file ->
              (* TODO: ../run-drun part should be gone *)
              let mo_file_path = Printf.sprintf "../run-drun/%s" mo_file in
              Printf.printf "Compiling mo file: %s\n" mo_file_path;

              let mo_base =
                Filename.remove_extension (Filename.basename mo_file)
              in
              let moc_cmd =
                Printf.sprintf "moc --hide-warnings -c %s -o %s/%s.drun.wasm\n"
                  mo_file_path out_dir mo_base
              in

              Printf.printf "Compiling %s: `%s`" mo_file moc_cmd;

              let mkdir_exit =
                Sys.command (Printf.sprintf "mkdir -p %s" out_dir)
              in
              Alcotest.(check int) "mkdir exit code" 0 mkdir_exit;

              let moc_exit = Sys.command moc_cmd in
              Alcotest.(check int) "moc exit code" 0 moc_exit)
            mo_files;

          (* Run drun, compare outputs *)
          let drun_script =
            prepare_drun_script drun_file_path out_dir test_name "drun"
          in

          Printf.printf "Drun script: %s\n" drun_script;

          let drun_processed_script_path =
            Printf.sprintf "%s/%s.drun.drun" out_dir test_name
          in
          write_file drun_processed_script_path drun_script;

          let actual_drun_output_path =
            Printf.sprintf "%s/%s.drun.out" out_dir test_name
          in
          let drun_cmd =
            Printf.sprintf "drun -c %s --extra-batches 1 %s > %s 2>&1"
              drun_config_path drun_processed_script_path
              actual_drun_output_path
          in

          Printf.printf "Running %s\n" drun_cmd;

          let drun_exit = Sys.command drun_cmd in
          Alcotest.(check int) "drun exit code" 0 drun_exit;

          let expected_drun_output_path =
            Printf.sprintf "../run-drun/ok/%s.drun.ok" test_name
          in

          let expected_output = read_file expected_drun_output_path in
          let actual_output = read_file actual_drun_output_path in

          let normalized_actual_output = normalize_drun_output actual_output in

          if expected_output <> normalized_actual_output then (
            let normalized_output_file_path =
              Printf.sprintf "%s/%s.drun.out.normal" out_dir test_name
            in

            write_file normalized_output_file_path normalized_actual_output;

            diff
              ~expected_output_path:
                (Printf.sprintf "../run-drun/ok/%s.drun.ok" test_name)
              ~actual_output_path:normalized_output_file_path));
    ] )

(** Scan directory drun/ for tests. Only the top-level files are tests. .drun
    files have .mo files in subdirectories. *)
let collect_drun_tests () : (string * unit Alcotest.test_case list) list =
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
  collect_drun_tests ()

let () =
  let open Alcotest in
  run "Motoko compiler tests" (collect_tests ())
